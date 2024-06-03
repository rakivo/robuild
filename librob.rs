//! # Robuild (rob)
//! # For example, this is how I build examples for my [virtual machine](https://github.com/rakivo/mm) with the [robuild](https://github.com/rakivo/robuild)
//! ```
//! use robuild::*;
//! use std::process::Output;
//!
//! const THREADS: &str = "-Z threads=10";
//! const LIB_FLAGS: &str = "--crate-type=rlib";
//! const DEBUG_FLAGS: &str = "-g -C \"opt-level=0\"";
//!
//! const BUILD: &str = "build";
//! const EXAMPLES: &str = "examples";
//!
//! fn main() -> IoResult::<()> {
//!     go_rebuild_yourself!(?);
//!
//!     Rob::mkdir(path!("examples", "build")).unwrap();
//!
//!     let mut rob = Rob::new();
//!     build_rakivo_mm(&mut rob)?;
//!     test_rakivo_mm(&mut rob)?;
//!
//!     Ok(())
//! }
//!
//! fn build_file(rob: &mut Rob, out: &str, name: &str, flags: &str) {
//!     let build_dir = path!(EXAMPLES, BUILD);
//!     let link_with_mm_flags: &str = &format!("--extern mm={path}", path = format!("{build_dir}/libmm.rlib"));
//!     rob.append(&["rustc", DEBUG_FLAGS, flags, THREADS, link_with_mm_flags, "-o",
//!                  &format!("{build_dir}/{out}"),
//!                  &path!(EXAMPLES, "mm", &format!("{name}.rs"))]);
//! }
//!
//! // Link to Rakivo's mm: https://github.com/rakivo/mm
//! fn build_rakivo_mm(rob: &mut Rob) -> IoResult::<Vec::<Output>> {
//!     build_file(rob, "libmm.rlib", "mm", LIB_FLAGS);
//!     build_file(rob, "load_from_binary", &path!("examples", "load_from_binary"), "");
//!     build_file(rob, "translate_masm", &path!("examples", "translate_masm"), "");
//!     rob.execute_all_sync()
//! }
//!
//! fn test_rakivo_mm(rob: &mut Rob) -> IoResult::<()> {
//!     use std::fs::read_to_string;
//!
//!     let build_dir = path!(EXAMPLES, BUILD);
//!     let output_path = format!("{build_dir}/fibm.out");
//!     let expected_path = path!(EXAMPLES, "mm", "load_from_binary.expected");
//!
//!     rob.append(&[&format!("{build_dir}/translate_masm"),
//!                  &path!(EXAMPLES, "mm", "fib.masm"),
//!                  &format!("{build_dir}/fibm")])
//!        .append(&[&format!("{build_dir}/load_from_binary"),
//!                   &format!("{p1} > {p2}", p1 = &format!("{build_dir}/fibm"), p2 = &output_path)])
//!        .execute_all_sync()?;
//!
//!     let output_string = read_to_string(&output_path)?;
//!     let expected_string = read_to_string(&expected_path)?;
//!     if output_string.trim() != expected_string.trim() {
//!         log!(PANIC, "Output of {output_path} doesn't equal to the expected one: {expected_path}");
//!     } else {
//!         log!(INFO, "TEST: `translate_masm`: OK");
//!     }
//!
//!     Ok(())
//! }
//! ```
//! All of the `execute`-like functions moving command-pointer forward,
//! if you wanna move the command-pointer manually, you can call the
//! `move_cmd_ptr` function which is especially implemented for `execute_all_sync`
//! and `execute_all_async` functions.
use std::{
    io::ErrorKind,
    time::SystemTime,
    default::Default,
    collections::VecDeque,
    path::{Path, PathBuf},
    fmt::{Display, Formatter},
    process::{exit, Command, Output, Stdio, Child},
    fs::{rename, metadata, read_dir, remove_file, create_dir_all, remove_dir_all},
};

pub const C_COMPILER: &str = if cfg!(feature = "gcc")     {"gcc"}
                        else if cfg!(feature = "clang")   {"clang"}
                        else if cfg!(feature = "mingw32") {"x86_64-w64-mingw32-gcc"}
                        else if cfg!(windows)             {"cl.exe"}
                        else                              {"cc"};
pub const CC: &str = C_COMPILER;

pub const CXX_COMPILER: &str = if cfg!(feature = "gxx")     {"g++"}
                          else if cfg!(feature = "clangxx") {"clang++"}
                          else if cfg!(feature = "mingw32") {"x86_64-w64-mingw32-g++"}
                          else if cfg!(windows)             {"cl.exe"}
                          else                              {"c++"};
pub const CXXC: &str = CXX_COMPILER;

pub const DELIM: &str = if cfg!(windows) {"\\"} else {"/"};
pub const CMD_ARG: &str = if cfg!(windows) {"cmd"} else {"sh"};
pub const CMD_ARG2: &str = if cfg!(windows) {"/C"} else {"-c"};

pub type IoResult<T> = std::io::Result::<T>;
pub type IoError = std::io::Error;

/// Call dis macro in your build recipe, and the program will
/// rebuild itself, freeing you from the need to rebuilding your
/// build recipe file
#[macro_export]
macro_rules! go_rebuild_yourself {
    () => {{
        let source_path = file!();
        let args = std::env::args().collect::<Vec::<_>>();
        Rob::go_rebuild_yourself(&args, &source_path).unwrap();
    }};
    (?) => {{
        let source_path = file!();
        let args = std::env::args().collect::<Vec::<_>>();
        Rob::go_rebuild_yourself(&args, &source_path)?;
    }}
}

/// You can log things just like we do in Rob functions,
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

/// Macro similar to the vec! macro, but produces
/// `std::path::Pathbuf` instead of `std::vec::Vec`
#[macro_export]
macro_rules! path {
    ($($p: expr), *) => {{
        let path = [$($p), *];
        path.join(DELIM)
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

macro_rules! colored {
    (r.$str: expr)  => { format!("\x1b[91m{}\x1b[0m", $str) };
    (y.$str: expr)  => { format!("\x1b[93m{}\x1b[0m", $str) };
    (br.$str: expr) => { format!("\x1b[31m{}\x1b[0m", $str) };
}

macro_rules! rob_gettable {
    ($name1: tt, $name2: tt, $name3: tt, $stack: tt, $type: ty) => {
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
        pub fn $name1(&mut self) -> Option::<$type> {
            self.$stack.pop_front()
        }

        #[inline]
        pub fn $name2(&self) -> Vec::<&$type> {
            self.$stack.iter().collect()
        }

        #[inline]
        pub fn $name3(self) -> Vec::<$type> {
            self.$stack.into_iter().collect()
        }
    };
}

/// Structure for convenient work with directories.
#[derive(Debug)]
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

#[derive(Debug)]
pub enum LogLevel {
    CMD,
    INFO,
    WARN,
    ERROR,
    PANIC
}

impl Display for LogLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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

/// Structure for executing commands (actually just keeping them, but it's just for now)
#[derive(Debug, Default)]
pub struct RobCommand {
    lines: Vec::<Vec::<String>>,
}

/// The main `Rob` structure.
#[derive(Debug, Default)]
pub struct Rob {
    cmd: RobCommand,
    cp: usize,
    echo: bool,
    keepgoing: bool,
    child_stack: VecDeque::<Child>,
    output_stack: VecDeque::<Output>
}

impl Rob {
    pub fn new() -> Rob {
        Rob {
            echo: true,
            ..Rob::default()
        }
    }

    /// Checks if src file needs rebuild
    pub fn needs_rebuild(bin: &str, src: &str) -> IoResult::<bool> {
        if !Rob::path_exists(bin) { return Ok(true) }
        let bin_mod_time = Rob::get_last_modification_time(bin)?;
        let src_mod_time = Rob::get_last_modification_time(src)?;
        Ok(src_mod_time > bin_mod_time)
    }

    pub fn needs_rebuild_many(bin: &str, srcs: &Vec::<&str>) -> IoResult::<bool> {
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
    /// Modified text from: `https://github.com/tsoding/nobuild`
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
    pub fn go_rebuild_yourself(args: &Vec::<String>, source_path: &str) -> IoResult::<()> {
        assert!(args.len() >= 1);
        let binary_pathbuf = std::env::current_exe()?;
        let binary_path = binary_pathbuf.to_str().to_owned().unwrap();
        let rebuild_is_needed = Rob::needs_rebuild(&binary_path, source_path)?;
        if rebuild_is_needed {
            let old_bin_path = format!("{binary_path}.old");
            log!(INFO, "RENAMING: {binary_path} -> {old_bin_path}");
            Rob::rename(binary_path, &old_bin_path)?;

            let self_compilation_output = Rob::new()
                .append(&["rustc -o", binary_path, source_path])
                .execute_sync();

            match self_compilation_output {
                Ok(out) => if !out.status.success() {
                    let stderr = String::from_utf8_lossy(&out.stderr);
                    log!(ERROR, "{stderr}");
                    let code = out.status.code()
                        .expect("Process terminated by signal");
                    log!(ERROR, "Compilation exited abnormally with code: {code}");
                    Rob::rename(old_bin_path.as_str(), binary_path)?;
                    exit(1);
                }
                Err(err) => {
                    log!(ERROR, "Failed to rename file: {old_bin_path}: {err}");
                    Rob::rename(old_bin_path.as_str(), binary_path)?;
                    exit(1);
                }
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
            PANIC => unreachable!()
        }
    }

    /// Takes path and returns it without the file extension
    #[inline]
    pub fn noext(p: &str) -> String {
        p.chars().take_while(|x| *x != '.').collect()
    }

    /// Appends arguments to the last line in cmd.lines,
    /// ```
    /// let p = pathbuf!["dummy", "rakivo", "dummy.cpp"];
    /// Rob::new()
    ///     .append(&["clang++", "-o", "output", p.to_str().unwrap()])
    ///     .execute()
    /// ```
    /// It Outputs:
    /// ```
    /// [CMD] clang++ -o output test/test1/test.cpp
    /// ```
    #[inline]
    pub fn append<S>(&mut self, args: &[S]) -> &mut Self
    where
        S: AsRef<str>
    {
        // The unwrap can possibly fail ONLY if cmd.lines (std::vec::Vec) has overflowed
        let cmd = args.iter().map(|s| s.as_ref().to_string()).collect::<Vec<_>>();
        self.cmd.lines.push(cmd);
        self
    }

    fn format_out(out: &str) -> &str {
        if out.ends_with('\n') {
            &out[0..out.len() - 1]
        } else {
            out
        }
    }

    fn render_output(out: &Output, echo: &bool) {
        if out.status.success() {
            let stdout = String::from_utf8_lossy(&out.stdout);
            if !stdout.is_empty() && *echo {
                let formatted = Rob::format_out(&stdout);
                log!(INFO, "{formatted}");
            }
        } else {
            let stderr = String::from_utf8_lossy(&out.stderr);
            if !stderr.is_empty() && *echo {
                let formatted = Rob::format_out(&stderr);
                log!(ERROR, "{formatted}");
            }
        }
    }

    #[inline]
    pub fn echo(&mut self, echo: bool) -> &mut Self {
        self.echo = echo;
        self
    }

    #[inline]
    pub fn keepgoing(&mut self, keepgoing: bool) -> &mut Self {
        self.keepgoing = keepgoing;
        self
    }

    #[inline]
    pub fn move_cmd_ptr(&mut self) -> &mut Self {
        self.cp += 1;
        self
    }

    rob_gettable!(output, outputs_refs, outputs, output_stack, Output);
    rob_gettable!(child, children_refs, children, child_stack, Child);

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
        let out = self.execute_sync_helper()?;
        println!("{out:?}");
        self.output_stack.push_back(out);
        Ok(self)
    }

    pub fn execute_and_get_output(&mut self) -> IoResult::<Output> {
        let out = self.execute_sync_helper()?;
        Ok(out)
    }

    fn execute_sync_helper(&mut self) -> IoResult::<Output> {
        let Some(args) = self.get_args()
        else {
            let err = IoError::new(ErrorKind::Other, "No arguments to process");
            return Err(err)
        };

        if self.echo {
            log!(CMD, "{args}");
        }

        let mut cmd = Command::new(CMD_ARG);
        cmd.arg(CMD_ARG2).arg(args);

        if !self.echo {
            cmd.stdout(Stdio::null())
               .stderr(Stdio::null());
        }

        let out = cmd.output()?;

        if !self.keepgoing && !out.status.success() {
            let code = out.status.code()
                .expect("Process terminated by signal");

            let stderr = String::from_utf8_lossy(&out.stderr);
            log!(ERROR, "{stderr}");
            log!(ERROR, "Compilation exited abnormally with code: {code}");
            exit(1);
        }

        self.cp += 1;
        Rob::render_output(&out, &self.echo);
        Ok(out)
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_sync(&mut self) -> IoResult::<Vec::<Output>> {
        let mut outs = Vec::new();
        for line in self.cmd.lines.iter() {
            let args = line.join(" ");

            if self.echo { log!(CMD, "{args}"); }
            let mut cmd = Command::new(CMD_ARG);
            cmd.arg(CMD_ARG2).arg(args);

            if !self.echo {
                cmd.stdout(Stdio::null())
                   .stderr(Stdio::null());
            }

            let out = cmd.output()?;

            if !self.keepgoing && !out.status.success() {
                let code = out.status.code()
                    .expect("Process terminated by signal");

                let stderr = String::from_utf8_lossy(&out.stderr);
                log!(ERROR, "{stderr}");
                log!(ERROR, "Compilation exited abnormally with code: {code}");
                exit(1);
            }

            outs.push(out);
        }
        Ok(outs)
    }

    #[inline]
    fn get_args(&self) -> Option::<String> {
        if let Some(args) = self.cmd.lines.get(self.cp) {
            Some(args.join(" "))
        } else { None }
    }

    /// Blocking operation.
    ///
    /// Simply creates `Command` for each line in self.lines and executes them.
    /// More about that: `https://doc.rust-lang.org/std/process/struct.Command.html`
    pub fn execute_sync(&mut self) -> IoResult::<Output> {
        let out = self.execute_sync_helper()?;
        self.cmd.lines.push(Vec::new());
        Ok(out)
    }

    fn execute_async_helper(&mut self) -> IoResult::<Child> {
        let Some(args) = self.get_args()
        else {
            let err = IoError::new(ErrorKind::Other, "No arguments to process");
            return Err(err)
        };

        if self.echo {
            log!(CMD, "{args}");
        }

        let child = Command::new(CMD_ARG)
            .arg(CMD_ARG2)
            .arg(args)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;

        self.cp += 1;
        Ok(child)
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_async(&mut self) -> IoResult::<Vec::<Child>> {
        let mut children = Vec::new();
        for line in self.cmd.lines.iter() {
            let args = line.join(" ");

            if self.echo { log!(CMD, "{args}"); }
            let mut cmd = Command::new(CMD_ARG);
            cmd.arg(CMD_ARG2).arg(&args);

            let child = Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(args)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()?;

            children.push(child);
        }
        Ok(children)
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_async_and_wait(&mut self) -> IoResult::<()> {
        let children = self.execute_all_async()?;
        Rob::wait_for_children_deq(children.into(), &self.echo)?;
        Ok(())
    }

    /// Non-Blocking operation.
    ///
    /// It is a second version of the `Rob::execute_async_child` function, especially
    /// to use in one line. For instance:
    /// ```
    /// Rob::new()
    ///     .append(&["clang", "-o build/output", "./test/main.c"])
    ///     .execute_async()?
    ///     .append(&["clang++", "-o build/outputpp", "./test/main.cpp"])
    ///     .execute_async()?;
    /// ```
    /// After executing command, its child process will be pushed into the child_stack.
    /// If you need, you can get the output via calling the `Rob::get_child` function.
    pub fn execute_async(&mut self) -> IoResult::<&mut Self> {
        let child = self.execute_async_helper()?;
        self.child_stack.push_back(child);
        Ok(self)
    }

    /// Blocks the main thread and waits for all of the children.
    pub fn wait_for_children_deq(mut children: VecDeque::<Child>, echo: &bool) -> IoResult::<Vec::<Output>> {
        let mut ret = Vec::new();
        while let Some(child) = children.pop_front() {
            let out = Rob::wait_for_child(child)?;
            Rob::render_output(&out, echo);
            ret.push(out);
        } Ok(ret)
    }

    /// ! MOVES SELF !
    /// Blocks the main thread and waits for all of the children.
    pub fn wait_for_children(self) -> IoResult::<Vec::<Output>> {
        Rob::wait_for_children_deq(self.child_stack, &self.echo)
    }

    /// Blocks the main thread and waits for the child.
    #[inline]
    pub fn wait_for_child(child: Child) -> IoResult::<Output> {
        child.wait_with_output()
    }
}

/*
More important TODOs:
    (#1): Introduce job system.
    Because you need to have an ability to
    say to the rob, that something need to be
    compiled before something else ykwim.

    (#2): Change the command ptr system.
    Because you can't combine execute single line function,
    and execute all functions, everything breakes because of that system,
    maybe we need use VecDeque instead of Vec and pop lines,
    that were executed or something like that.

Less important TODOs:
    README;
    Examples;
    Other Nob features;
    Documentation;
*/
