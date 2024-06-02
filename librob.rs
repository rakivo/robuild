use std::{
    fs::{
        metadata,
        create_dir_all,
        read_dir,
        remove_dir_all,
        remove_file,
        rename
    },
    any::Any,
    time::SystemTime,
    collections::VecDeque,
    path::{Path, PathBuf},
    thread::{self, JoinHandle},
    io::{ErrorKind, Result as ioResult},
    process::{exit, Command, Output, Stdio},
};

const CMD_ARG: &str = if cfg!(windows) {"cmd"} else {"sh"};
const CMD_ARG2: &str = if cfg!(windows) {"/C"} else {"-c"};

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

#[macro_export]
macro_rules! pathbuf {
    ($($p: expr), *) => {{
        let mut path = std::path::PathBuf::new();
        $(path.push($p);)*
        path
    }}
}

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

pub struct RobCommand {
    lines: Vec::<String>,
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

impl RobCommand {
    #[inline]
    pub fn new() -> RobCommand {
        Self { lines: Vec::new() }
    }
}

pub struct Rob(RobCommand);

impl Rob {
    #[inline]
    pub fn new() -> Rob {
        Rob(RobCommand::new())
    }

    fn needs_rebuild(bin: &str, srcs: &Vec::<&str>) -> ioResult::<bool> {
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
    pub fn go_rebuild_yourself(args: &Vec::<String>, source_path: &str) -> ioResult::<()> {
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
                Ok(_) => exit(0),
                Err(err) => {
                    log!(ERROR, "FAILED TO RESTART ROB FROM FILE: {binary_path}: {err}");
                    exit(1);
                }
            }
        } Ok(())
    }

    #[inline]
    pub fn get_last_modification_time<P>(p: P) -> ioResult::<SystemTime>
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
    pub fn rename<P>(from: P, to: P) -> ioResult<()>
    where
        P: AsRef::<Path>
    {
        rename(from, to)
    }

    #[inline]
    pub fn mkdir<P>(p: P) -> ioResult<()>
    where
        P: Into::<PathBuf>
    {
        create_dir_all(p.into())
    }

    pub fn rm<P>(p: P) -> ioResult<()>
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
    fn panic(out: &str) -> ! {
        panic!("{out}", out = out.to_owned())
    }

    fn log(lvl: LogLevel, out: &str, f: &str, l: u32, c: u32) {
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

    /* TODO:
    Change the way how we treating lines, i think we should introduce
    some new function like `execute_last_sync` or something, that just moves command pointer
    therefore following input will be treated as next line and so on, if ykwim
    */
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
    /// [INFO] [EMPTY]
    /// ```
    ///
    /// Output is [EMPTY] because the program compiled successfully.
    #[inline]
    pub fn append<'a, I, S>(&mut self, args: I) -> &mut Self
    where
        I: IntoIterator::<Item = S>,
        S: AsRef<str>
    {
        let line = args.into_iter()
            .map(|arg| arg.as_ref().to_owned())
            .collect::<Vec<_>>()
            .join(" ");

        self.0.lines.push(line);
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

    /// Blocking operation.
    ///
    /// Simply creates `Command` for each line in self.lines and executes them.
    /// More about that: https://doc.rust-lang.org/std/process/struct.Command.html
    pub fn execute_sync(&mut self) -> ioResult::<Vec::<Output>> {
        let mut ret = Vec::new();

        for line in self.0.lines.iter() {
            log!(CMD, "{line}");
            let out = Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(&line)
                .output()?;

            Self::process_output(&out);
            ret.push(out);
        } Ok(ret)
    }

    /// Non-blocking operation.
    ///
    /// Created `Command` for each line in self.lines.
    /// More about that: https://doc.rust-lang.org/std/process/struct.Command.html
    ///
    /// Stdout and stderr are piped to manage all of the outputs properly.
    /// Returns handle that waits for each child and contains vector of the outputs.
    ///
    /// To conveniently wait for the returned children you can use Rob::wait_for_children
    pub fn execute_async(&mut self) -> ioResult::<JoinHandle::<Vec::<Output>>> {
        let mut children = Vec::new();
        for line in self.0.lines.iter() {
            log!(CMD, "{line}");

            let out = Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(&line)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?;

            children.push(out);
        }

        let handle = thread::spawn(move || {
            children.into_iter().map(|child| {
                child.wait_with_output().map_err(|err| {
                    log!(PANIC, "FAILED TO WAIT FOR CHILD: {err}");
                    err
                }).unwrap()
            }).collect()
        });

        Ok(handle)
    }

    /// Blocks the main thread and waits for all of the children.
    pub fn wait_for_children<E>(handle: JoinHandle::<Vec::<ioResult::<Output>>>) -> Result<(), E>
    where
        E: From::<Box<dyn Any + Send>>
    {
        handle.join()?
            .iter()
            .try_for_each(|res| {
                if let Ok(out) = res {
                    Rob::process_output(out);
                } Ok(())
            })
    }
}

/* TODO:
    Examples,
    Other Nob features,
    Documentation,
*/
