use std::{
    fs::{
        metadata,
        create_dir_all,
        read_dir,
        remove_dir_all,
        remove_file,
        rename
    },
    io::ErrorKind,
    time::SystemTime,
    collections::VecDeque,
    path::{Path, PathBuf},
    process::{exit, Command, Output, Stdio, Child},
};

pub const CMD_ARG: &str = if cfg!(windows) {"cmd"} else {"sh"};
pub const CMD_ARG2: &str = if cfg!(windows) {"/C"} else {"-c"};

pub type IoResult<T> = std::io::Result::<T>;

/// Call dis macro in your build recipe, and the program will
/// rebuild itself, freeing you from the need to rebuilding your
/// build recipe file
#[macro_export]
macro_rules! go_rebuild_yourself {
    () => {{
        let source_path = file!();
        let args = std::env::args().collect::<Vec::<_>>();
        Rob::go_rebuild_yourself(&args, &source_path).unwrap();
    }}
}

macro_rules! colored {
    (r.$str: expr)  => { format!("\x1b[91m{}\x1b[0m", $str) };
    (y.$str: expr)  => { format!("\x1b[93m{}\x1b[0m", $str) };
    (br.$str: expr) => { format!("\x1b[31m{}\x1b[0m", $str) };
}

/// You can log things just like we're logging in Rob functions,
/// pass the `LogLevel` enum variant and then format your output like you'r
/// using the `println`! macro. For instance:
/// ```
/// let what = "logging";
/// log!(INFO, "This is how you can do {what}");
/// ```
/// This will print:
/// ```
/// [INFO] This is how you can do logging
/// ```
#[macro_export]
macro_rules! log {
    ($log_level: tt, $($args: expr), *) => {{
        use LogLevel::*;
        let out = format!($($args), *);
        match $log_level {
            PANIC => {
                let out = format!("{lvl} {f}:{l}:{c}: {out}",
                                  lvl = LogLevel::$log_level, f = file!(),
                                  l = line!(), c = column!());
                Rob::panic(&out)
            }
            _ => Rob::log($log_level, &out, file!(), line!(), column!())
        }
    }}
}

/// Macro similar to the vec! macro, but produces
/// `std::path::Pathbuf` instead of `std::vec::Vec`
#[macro_export]
macro_rules! pathbuf {
    ($($p: expr), *) => {{
        let mut path = std::path::PathBuf::new();
        $(path.push($p);)*
        path
    }}
}

/// Just pass strs and it will create directories,
/// whether it nested or not. For instance:
/// ```
/// mkdirs!("just", "for", "example");
/// ```
/// Regardless of the target os will create `just` directory having in it
/// `for` and `example` directories.
#[macro_export]
macro_rules! mkdirs {
    ($($dir: expr), *) => {{
        let p = pathbuf![$($dir), *];
        Rob::mkdir(p)
    }}
}

pub struct Dir {
    stack: VecDeque::<PathBuf>,
}

impl Dir {
    /// Takes path to a directory and returns
    /// instance of the iterable struct `Dir`.
    ///
    /// `Dir` iterates using the BFS algorithm,
    ///
    /// if current element is file `Dir` returns it,
    /// otherwise it iterates that directory and checkes for other files.
    pub fn new<P>(root: P) -> Dir
    where
        P: Into::<PathBuf>
    {
        let mut stack = VecDeque::new();
        stack.push_back(root.into());
        Dir { stack }
    }
}

impl Iterator for Dir {
    type Item = PathBuf;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(p) = self.stack.pop_front() {
            if p.is_file() { return Some(p) }

            match read_dir(&p) {
                Ok(es) => es.filter_map(Result::ok).for_each(|e| {
                    self.stack.push_back(e.path())
                }),
                Err(e) => eprintln!("ERROR: {e}")
            }
        } None
    }
}

pub enum LogLevel {
    CMD,
    INFO,
    WARN,
    ERROR,
    PANIC
}

impl std::fmt::Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use self::LogLevel::*;
        match self {
            CMD   => write!(f, "[CMD]")?,
            INFO  => write!(f, "[INFO]")?,
            WARN  => write!(f, "[WARN]")?,
            ERROR => write!(f, "[ERROR]")?,
            PANIC => write!(f, "[PANIC]")?
        } Ok(())
    }
}

pub struct RobCommand {
    lines: Vec::<Vec::<String>>,
}

impl RobCommand {
    #[inline]
    pub fn new() -> RobCommand {
        Self { lines: vec![Vec::new()] }
    }
}

pub struct Rob {
    cmd: RobCommand,
    cp: usize,
    output_stack: VecDeque::<Output>
}

impl Rob {
    pub fn new() -> Rob {
        Rob {
            cmd: RobCommand::new(),
            cp: 0,
            output_stack: VecDeque::new()
        }
    }

    fn needs_rebuild(bin: &str, srcs: &Vec::<&str>) -> IoResult::<bool> {
        if !Rob::path_exists(bin) { return Ok(true) }

        let bin_mod_time = Rob::get_last_modification_time(bin)?;
        for src in srcs {
            let src_mod_time = Rob::get_last_modification_time(src)?;
            if src_mod_time > bin_mod_time {
                return Ok(true)
            }
        } Ok(false)
    }

    // The implementation idea is stolen from https://github.com/tsoding/nobuild,
    // which is also stolen from https://github.com/zhiayang/nabs
    /// Modified text from: https://github.com/tsoding/nobuild
    /// `Go Rebuild Urself™ Technology`
    ///
    /// How to use it:
    /// ```
    /// fn main() {
    ///     go_rebuild_yourself!();
    ///     // actual work
    /// }
    /// ```
    ///
    /// After your added this macro every time you run `./rob` it will detect
    /// that you modified its original source code and will try to rebuild itself
    /// before doing any actual work. So you only need to bootstrap your build system
    /// once.
    ///
    /// The modification is detected by comparing the last modified times of the executable
    /// and its source code. The same way the make utility usually does it.
    ///
    /// The rebuilding is done by using the `go_rebuild_yourself` macro which you can redefine
    /// if you need a special way of bootstraping your build system. (which I personally
    /// do not recommend since the whole idea of robuild is to keep the process of bootstrapping
    /// as simple as possible and doing all of the actual work inside of the nobuild)
    pub fn go_rebuild_yourself(args: &Vec::<String>, source_path: &str) -> IoResult::<()> {
        assert!(args.len() >= 1);
        let binary_path = &args[0];
        let rebuild_is_needed = Rob::needs_rebuild(&binary_path, &vec![source_path])?;
        if rebuild_is_needed {
            let old_bin_path = format!("{binary_path}.old");
            log!(INFO, "RENAMING: {binary_path} -> {old_bin_path}");
            Rob::rename(binary_path, &old_bin_path)?;

            if let Err(err) = Rob::new()
                .append(&["rustc -o", binary_path, source_path])
                .execute_sync()
            {
                log!(ERROR, "FAILED TO RENAME FILE: {old_bin_path}: {err}");
                Rob::rename(&old_bin_path, binary_path)?;
                exit(1);
            }

            match Rob::new()
                .append(args)
                .execute_sync()
            {
                Ok(_) => {
                    log!(INFO, "REMOVING: {old_bin_path}");
                    Rob::rm_if_exists(old_bin_path);
                    exit(0);
                }
                Err(err) => {
                    log!(ERROR, "FAILED TO RESTART ROB FROM FILE: {binary_path}: {err}");
                    exit(1);
                }
            }
        } Ok(())
    }

    #[inline]
    pub fn get_last_modification_time<P>(p: P) -> IoResult::<SystemTime>
    where
        P: AsRef::<Path>
    {
        metadata(p)?.modified()
    }

    #[inline]
    pub fn is_file<P>(p: P) -> bool
    where
        P: Into::<PathBuf>
    {
        p.into().is_file()
    }

    #[inline]
    pub fn is_dir<P>(p: P) -> bool
    where
        P: Into::<PathBuf>
    {
        p.into().is_dir()
    }

    #[inline]
    pub fn path_exists<P>(p: P) -> bool
    where
        P: Into::<PathBuf>
    {
        p.into().exists()
    }

    #[inline]
    pub fn rename<P>(from: P, to: P) -> IoResult<()>
    where
        P: AsRef::<Path>
    {
        rename(from, to)
    }

    #[inline]
    pub fn mkdir<P>(p: P) -> IoResult::<()>
    where
        P: Into::<PathBuf>
    {
        create_dir_all(p.into())
    }

    pub fn rm_if_exists<P>(p: P)
    where
        P: Into::<PathBuf> + ToOwned::<Owned = String>
    {
        if Rob::is_dir(p.to_owned()) {
            remove_dir_all(p.into()).expect("Failed to remove directory")
        } else if Rob::is_file(p.to_owned()) {
            remove_file(p.into()).expect("Failed to remove file")
        }
    }

    pub fn rm<P>(p: P) -> IoResult::<()>
    where
        P: Into::<PathBuf> + ToOwned::<Owned = String>
    {
        if !Rob::path_exists(p.to_owned()) {
            return Err(ErrorKind::NotFound.into())
        } else if Rob::is_dir(p.to_owned()) {
            remove_dir_all(p.into())
        } else if Rob::is_file(p.to_owned()) {
            remove_file(p.into())
        } else {
            Err(ErrorKind::InvalidData.into())
        }
    }

    #[inline]
    pub fn panic(out: &str) -> ! {
        panic!("{out}", out = out.to_owned())
    }

    pub fn log(lvl: LogLevel, out: &str, f: &str, l: u32, c: u32) {
        use self::LogLevel::*;
        let out_with_info = format!("{f}:{l}:{c}: {out}");
        match lvl {
            CMD   => println!("{lvl} {out}"),
            INFO  => println!("{lvl} {out}"),
            WARN  => println!("{lvl} {out_with_info}", lvl = colored!(y."[WARN]")),
            ERROR => println!("{lvl} {out_with_info}", lvl = colored!(r."[ERROR]")),
            PANIC => panic!("UNREACHABLE")
        }
    }

    /// Takes path and returns it without the file extension
    #[inline]
    pub fn noext(p: &str) -> String {
        p.chars().take_while(|x| *x != '.').collect()
    }

    /// Takes all of the args and pushes them to self.lines,
    /// Each call of the append function will be treated as one line of arguments, as in here:
    /// ```
    /// let p = pathbuf!["dummy", "rakivo", "dummy.cpp"];
    /// let handle = Rob::new()
    ///     .append(&["clang++", "-o", "output", p.to_str().unwrap()])
    ///     .execute_sync();
    /// ```
    /// It Outputs:
    /// ```
    /// [CMD] clang++ -o output test/test1/test.cpp
    /// ```
    #[inline]
    pub fn append<I, S>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator::<Item = S>,
        S: AsRef<str>
    {
        let args = args.into_iter()
            .map(|arg| arg.as_ref().to_owned())
            .collect::<Vec<_>>();

        self.cmd.lines.last_mut().unwrap().extend(args);
        self
    }

    fn format_out(out: &str) -> &str {
        if out.ends_with('\n') {
            &out[0..out.len() - 1]
        } else {
            out
        }
    }

    fn process_output(out: &Output) {
        if out.status.success() {
            let stdout = String::from_utf8_lossy(&out.stdout);
            if !stdout.is_empty() {
                let formatted = Rob::format_out(&stdout);
                log!(INFO, "{formatted}");
            }
        } else {
            let stderr = String::from_utf8_lossy(&out.stderr);
            if !stderr.is_empty() {
                let formatted = Rob::format_out(&stderr);
                log!(ERROR, "{formatted}");
            }
        }
    }

    /// Function for receiving outputs of the commands. For instance:
    /// ```
    /// let mut rob = Rob::new();
    ///
    /// rob
    ///     .append(&["echo hello"])
    ///     .execute()?
    ///     .append(&["clang", "-o build/output", "./test/main.c"])
    ///     .execute()?
    ///     .append(&["clang++", "-o build/outputpp", "./test/main.cpp"])
    ///     .execute()?
    ///     .append(&["echo byebye"])
    ///     .execute()?;
    ///
    /// while let Some(out) = rob.output() {
    ///     println!("{out:?}");
    /// }
    /// ```
    /// Will print:
    /// ```
    /// [CMD] echo hello
    /// [INFO] hello
    /// [CMD] clang -o build/output ./test/main.c
    /// [CMD] clang++ -o build/outputpp ./test/main.cpp
    /// [CMD] echo byebye
    /// [INFO] byebye
    /// Output { status: ExitStatus(unix_wait_status(0)), stdout: "hello\n", stderr: "" }
    /// Output { status: ExitStatus(unix_wait_status(0)), stdout: "", stderr: "" }
    /// Output { status: ExitStatus(unix_wait_status(0)), stdout: "", stderr: "" }
    /// Output { status: ExitStatus(unix_wait_status(0)), stdout: "byebye\n", stderr: "" }
    /// ```
    /// As you can see, you receiving outputs in the reversed order, i think this is the best way of doing that.
    #[inline]
    pub fn output(&mut self) -> Option::<Output> {
        self.output_stack.pop_front()
    }

    #[inline]
    pub fn outputs_refs(&self) -> Vec::<&Output> {
        self.output_stack.iter().collect()
    }

    #[inline]
    pub fn outputs(self) -> Vec::<Output> {
        self.output_stack.into_iter().collect()
    }

    /// Blocking operation.
    ///
    /// It is a second version of the `Rob::execute_sync` function, especially
    /// to use in one line. For instance:
    /// ```
    /// Rob::new()
    ///     .append(&["clang", "-o build/output", "./test/main.c"])
    ///     .execute()?
    ///     .append(&["clang++", "-o build/outputpp", "./test/main.cpp"])
    ///     .execute()?;
    /// ```
    /// After executing command, its output will be pushed into the output_stack.
    /// If you need, you can get the output via calling the `Rob::get_output` function.
    pub fn execute(&mut self) -> IoResult::<&mut Self> {
        let args = self.cmd.lines[self.cp].join(" ");

        log!(CMD, "{args}");
        let out = Command::new(CMD_ARG)
            .arg(CMD_ARG2)
            .arg(&args)
            .output()?;

        Self::process_output(&out);

        self.cmd.lines.push(Vec::new());
        self.cp += 1;
        self.output_stack.push_back(out);
        Ok(self)
    }

    /// Blocking operation.
    ///
    /// Simply creates `Command` for each line in self.lines and executes them.
    /// More about that: `https://doc.rust-lang.org/std/process/struct.Command.html`
    pub fn execute_sync(&mut self) -> IoResult::<Output> {
        let args = self.cmd.lines[self.cp].join(" ");

        log!(CMD, "{args}");
        let out = Command::new(CMD_ARG)
            .arg(CMD_ARG2)
            .arg(&args)
            .output()?;

        Self::process_output(&out);
        self.cmd.lines.push(Vec::new());
        self.cp += 1;
        Ok(out)
    }

    /// Non-blocking operation.
    ///
    /// Created `Command` for each line in self.lines.
    /// More about that: `https://doc.rust-lang.org/std/process/struct.Command.html`
    ///
    /// Stdout and stderr are piped to manage all of the outputs properly.
    pub fn execute_async(&mut self) -> IoResult::<Child> {
        let args = self.cmd.lines[self.cp].join(" ");

        log!(CMD, "{args}");
        let child = Command::new(CMD_ARG)
            .arg(CMD_ARG2)
            .arg(&args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        self.cmd.lines.push(Vec::new());
        self.cp += 1;
        Ok(child)
    }
}

/* TODO:
    Examples,
    Other Nob features,
    Documentation,
*/
