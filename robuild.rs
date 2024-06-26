use std::{
    env,
    result,
    io::ErrorKind,
    time::SystemTime,
    default::Default,
    hash::{Hash, Hasher},
    collections::VecDeque,
    path::{Path, PathBuf},
    fmt::{Display, Formatter},
    process::{exit, Command, Output, Stdio, Child},
    fs::{rename, metadata, read_dir, remove_file, create_dir_all, remove_dir_all}
};

pub const C_COMPILER: &str = if cfg!(feature = "gcc")     {"gcc"}
else if cfg!(feature = "clang")   {"clang"}
else if cfg!(feature = "mingw32") {"x86_64-w64-mingw32-gcc"}
else if cfg!(windows)             {"cl.exe"}
else                              {"cc"};

pub const CXX_COMPILER: &str = if cfg!(feature = "gxx")     {"g++"}
else if cfg!(feature = "clangxx") {"clang++"}
else if cfg!(feature = "mingw32") {"x86_64-w64-mingw32-g++"}
else if cfg!(windows)             {"cl.exe"}
else                              {"c++"};

pub const CC:       &str = C_COMPILER;
pub const CXXC:     &str = CXX_COMPILER;

pub const DELIM:    &str   = if cfg!(windows) {"\\"} else {"/"};
pub const DELIM_CHAR: char = if cfg!(windows) {'\\'} else {'/'};

pub const CMD_ARG:  &str = if cfg!(windows) {"cmd"} else {"sh"};
pub const CMD_ARG2: &str = if cfg!(windows) {"/C"} else {"-c"};

/// Special symbol that you can put at the end of your slice while passing it
/// to the `append` function to perform `append` and move `acp ptr` simultaneously.
/// But I recommend you to use the `append_mv` function instead, it does the same but,
/// you don't need to put anything special at the end of the slice.
pub const MOVE_ACP_PTR_SYMBOL: &str = ".n";
pub const MAP: &str = MOVE_ACP_PTR_SYMBOL;

pub type IoError = std::io::Error;
pub type IoResult<T> = std::io::Result::<T>;

/// Call dis macro in your build recipe, and the program will
/// rebuild itself, freeing you from the need to rebuilding your
/// build recipe file
#[macro_export]
macro_rules! go_rebuild_yourself {
    () => {{
        let source_path = file!();
        Rob::go_rebuild_yourself(&source_path).unwrap();
    }};
    (?) => {{
        let source_path = file!();
        Rob::go_rebuild_yourself(&source_path)?;
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
    ($log_level: tt, $($args: tt)*) => {{
        #[allow(unused)]
        use LogLevel::*;
        Rob::log($log_level, &format!($($args)*));
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
    ($f: expr, r.$str: expr)  => { write!($f, "\x1b[91m{}\x1b[0m", $str) };
    ($f: expr, y.$str: expr)  => { write!($f, "\x1b[93m{}\x1b[0m", $str) };
    ($f: expr, br.$str: expr) => { write!($f, "\x1b[31m{}\x1b[0m", $str) };
}

/// Structure for convenient work with directories.
#[derive(Debug)]
pub struct DirRec {
    stack: VecDeque::<PathBuf>,
}

impl DirRec {
    /// Takes path to a directory and returns
    /// instance of the iterable struct `DirRec`.
    ///
    /// `DirRec` iterates using the BFS algorithm,
    ///
    /// if current element is file `DirRec` returns it,
    /// otherwise it iterates that directory and checkes for other files.
    pub fn new<P>(root: P) -> DirRec
    where
        P: Into::<PathBuf>
    {
        let mut stack = VecDeque::new();
        stack.push_back(root.into());
        DirRec {stack}
    }
}

impl Iterator for DirRec {
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

/// Structure for convenient non-recursive work with directories.
#[derive(Debug)]
pub struct Dir {
    paths: Vec::<PathBuf>,
}

impl Dir {
    /// Takes path to a directory and returns
    /// instance of the iterable struct `Dir`.
    ///
    /// `Dir` iterates using the BFS algorithm,
    ///
    /// Unlike `DirRec`, `Dir` iterates only thru each ! File ! in specified directory,
    /// and does not iterates all of nested directories recursively.
    pub fn new<P>(root: P) -> Dir
    where
        P: Into::<PathBuf>
    {
        Dir {
            paths: if let Ok(entries) = read_dir(root.into()) {
                entries.into_iter()
                    .filter_map(Result::ok)
                    .filter(|e| e.path().is_file())
                    .map(|e| e.path())
                    .collect()
            } else {
                Vec::new()
            }
        }
    }
}

impl IntoIterator for Dir {
    type Item = PathBuf;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.paths.into_iter()
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
            WARN  => colored!(f, y."[WARN]")?,
            ERROR => colored!(f, br."[ERROR]")?,
            PANIC => colored!(f, r."[PANIC]")?
        } Ok(())
    }
}

/// Structure for executing commands (actually just keeping them, but it's just for now)
#[derive(Eq, Debug, Clone, PartialEq)]
pub struct RobCommand {
    lines: Vec::<Vec::<String>>,
    acp: usize, // append command pointer
    ecp: usize, // execution command pointer
    pub cfg: Config,
    output_stack: VecDeque::<Output>,
}

impl Hash for RobCommand {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.lines.hash(state);
        self.acp.hash(state);
        self.ecp.hash(state);
        self.cfg.hash(state);
    }
}

impl From<Config> for RobCommand {
    fn from(cfg: Config) -> Self {
        Self {
            cfg, ..Self::default()
        }
    }
}

impl From<Vec::<Vec::<String>>> for RobCommand {
    fn from(lines: Vec::<Vec::<String>>) -> Self {
        Self {
            lines,
            ..Self::default()
        }
    }
}

impl Default for RobCommand {
    fn default() -> Self {
        Self {
            lines: vec![Vec::new()],
            acp: 0, ecp: 0,
            cfg: Config::default(),
            output_stack: VecDeque::new()
        }
    }
}

impl RobCommand {
    pub fn new() -> RobCommand {
        RobCommand::default()
    }

    /// Appends arguments to the last line in cmd,
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
    pub fn append<S>(&mut self, xs: &[S]) -> &mut Self
    where
        S: ToString + PartialEq::<&'static str>
    {
        // check for the `move_acp_ptr` symbol
        if matches!(xs.last(), Some(last) if last == &MAP) {
            let args = xs[0..xs.len() - 1].into_iter().map(S::to_string).collect::<Vec::<_>>();
            self.lines[self.acp].extend(args);
            self.move_acp_ptr();
        } else {
            let args = xs.into_iter().map(S::to_string).collect::<Vec::<_>>();
            self.lines[self.acp].extend(args);
        } self
    }

    /// Performs append and moves append ptr forward
    pub fn append_mv<S>(&mut self, xs: &[S]) -> &mut Self
    where
        S: ToString + PartialEq::<&'static str>
    {
        self.append(xs);
        self.move_acp_ptr();
        self
    }

    #[inline]
    pub fn move_acp_ptr(&mut self) -> &mut Self {
        self.acp += 1;
        self.lines.push(Vec::new());
        self
    }

    pub fn execute(&mut self) -> IoResult::<&mut Self> {
        let out = self.execute_sync()?;
        if let Some(last) = self.lines.last_mut() {
            *last = Vec::new();
        }
        self.output_stack.push_back(out);
        Ok(self)
    }

    #[inline]
    pub fn execute_sync(&mut self) -> IoResult::<Output> {
        self.execute_sync_exit(true)
    }

    #[inline]
    pub fn execute_sync_dont_exit(&mut self) -> IoResult::<Output> {
        self.execute_sync_exit(false)
    }

    fn execute_sync_exit(&mut self, exit_: bool) -> IoResult::<Output> {
        let Some(args) = self.get_args()
        else {
            let err = IoError::new(ErrorKind::Other, "No arguments to process");
            return Err(err)
        };

        if self.cfg.echo { log!(CMD, "{args}"); }
        let mut cmd = Command::new(CMD_ARG);
        cmd.arg(CMD_ARG2).arg(args);

        if !self.cfg.echo {
            cmd.stdout(Stdio::null())
                .stderr(Stdio::null());
        }

        let out = cmd.output()?;

        if !self.cfg.keepgoing && !out.status.success() {
            let code = out.status.code()
                .expect("Process terminated by signal");

            let stderr = String::from_utf8_lossy(&out.stderr);
            log!(ERROR, "{stderr}");
            log!(ERROR, "Compilation exited abnormally with code: {code}");

            if exit_ {
                exit(1);
            }
        }
        Ok(out)
    }

    #[inline]
    pub fn execute_all_sync(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute_all_sync_exit(true)
    }

    #[inline]
    pub fn execute_all_sync_dont_exit(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute_all_sync_exit(false)
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_sync_exit(&mut self, exit_: bool) -> RobResult::<Vec::<Output>> {
        let mut outs = Vec::new();
        for idx in self.ecp..self.lines.len() {
            let line = &self.lines[idx];
            let args = line.join(" ");
            if args.is_empty() { continue }

            if self.cfg.echo { log!(CMD, "{args}"); }
            let mut cmd = Command::new(CMD_ARG);
            cmd.arg(CMD_ARG2).arg(args);

            if !self.cfg.echo {
                cmd.stdout(Stdio::null())
                   .stderr(Stdio::null());
            }

            let out = cmd.output().map_err(|err| {
                RobError::FailedToGetOutput(err)
            })?;

            if !self.cfg.keepgoing && !out.status.success() {
                let code = out.status.code()
                    .expect("Process terminated by signal");

                let stderr = String::from_utf8_lossy(&out.stderr);
                log!(ERROR, "{stderr}");
                log!(ERROR, "Compilation exited abnormally with code: {code}");

                if exit_ {
                    exit(1);
                }
            }

            self.ecp += 1;
            outs.push(out);
        }
        Ok(outs)
    }

    #[inline]
    fn get_args(&self) -> Option::<String> {
        if let Some(args) = self.lines.last() {
            if args.is_empty() { None }
            else               { Some(args.join(" ")) }
        } else { None }
    }

    /// Function for receiving output of the last executed command.
    /// ```
    /// let mut rob = Rob::new();
    ///
    /// rob
    ///     .append(&["echo hello"])
    ///     .execute()?
    ///     .append(&[CC, "-o build/output", "./test/main.c"])
    ///     .execute()?
    ///     .append(&[CXXC, "-o build/outputpp", "./test/main.cpp"])
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
    pub fn outputs_refs(&self) -> VecDeque::<&Output> {
        self.output_stack.iter().collect()
    }

    #[inline]
    pub fn outputs(self) -> VecDeque::<Output> {
        self.output_stack.into_iter().collect()
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_async(&mut self) -> RobResult::<Vec::<Child>> {
        let mut children = Vec::new();
        for idx in self.ecp..self.lines.len() {
            let line = &self.lines[idx];
            let args = line.join(" ");
            if args.is_empty() { continue }

            if self.cfg.echo { log!(CMD, "{args}"); }
            let mut cmd = Command::new(CMD_ARG);
            cmd.arg(CMD_ARG2).arg(&args);

            let child = Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(args)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .map_err(|err| RobError::FailedToSpawnChild(err))?;

            self.ecp += 1;
            children.push(child);
        }
        Ok(children)
    }

    fn format_out(out: &str) -> &str {
        if out.ends_with('\n') {
            &out[0..out.len() - 1]
        } else {
            &out[0..]
        }
    }

    #[inline]
    pub fn execute_all_async_and_wait(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute_all_async_and_wait_exit(true)
    }

    #[inline]
    pub fn execute_all_async_and_wait_dont_exit(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute_all_async_and_wait_exit(false)
    }

    /// Returns vector of child which you can turn into vector of the outputs using Rob::wait_for_children.
    pub fn execute_all_async_and_wait_exit(&mut self, exit: bool) -> RobResult::<Vec::<Output>> {
        let children = self.execute_all_async()?;
        Self::wait_for_children_deq(children.into(), &self.cfg, exit)
    }

    /// Blocks the main thread and waits for all of the children.
    pub fn wait_for_children_deq(mut children: VecDeque::<Child>, cfg: &Config, exit_: bool) -> RobResult::<Vec::<Output>> {
        let mut ret = Vec::new();
        while let Some(child) = children.pop_front() {
            let out = Self::wait_for_child(child).map_err(|err| {
                RobError::FailedToGetOutput(err)
            })?;

            if out.status.success() {
                let stdout = String::from_utf8_lossy(&out.stdout);
                if !stdout.is_empty() && cfg.echo {
                    let formatted = Self::format_out(&stdout);
                    log!(INFO, "{formatted}");
                }
            } else {
                let stderr = String::from_utf8_lossy(&out.stderr);
                if !stderr.is_empty() && cfg.echo {
                    let formatted = Self::format_out(&stderr);
                    log!(ERROR, "{formatted}");
                    if !cfg.keepgoing {
                        let code = out.status.code().expect("Process terminated by signal");
                        log!(ERROR, "Compilation exited abnormally with code: {code}");
                        if exit_ {
                            exit(1);
                        }
                    }
                }
            }

            ret.push(out);
        } Ok(ret)
    }

    /// Blocks the main thread and waits for the child.
    #[inline]
    pub fn wait_for_child(child: Child) -> IoResult::<Output> {
        child.wait_with_output()
    }

    #[inline]
    pub fn echo(&mut self, echo: bool) -> &mut Self {
        self.cfg.echo(echo);
        self
    }

    #[inline]
    pub fn keepgoing(&mut self, keepgoing: bool) -> &mut Self {
        self.cfg.keepgoing(keepgoing);
        self
    }
}

#[derive(Hash, Eq, Debug, Clone, PartialEq)]
pub struct Config {
    pub echo: bool,
    pub keepgoing: bool,
}

impl Default for Config {
    fn default() -> Self {
        Self{echo: true, keepgoing: false}
    }
}

impl Config {
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
}

#[derive(Hash, Eq, Debug, Clone, Default, PartialEq)]
pub struct Job {
    target: String,
    phony: bool,
    deps: Vec::<String>,
    cmd: RobCommand,
    reusable_cmd: bool
}

#[derive(Debug)]
pub enum RobError {
    NotFound(String),
    FailedToGetOutput(IoError),
    FailedToSpawnChild(IoError)
}

impl Display for RobError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use RobError::*;
        match self {
            NotFound(file_path)     => write!(f, "File not found: {file_path}"),
            FailedToGetOutput(err)  => write!(f, "Failed to get output: {err}", err = err.to_string()),
            FailedToSpawnChild(err) => write!(f, "Failed to spawn child: {err}", err = err.to_string()),
        }
    }
}

pub type RobResult<T> = result::Result::<T, RobError>;

impl Job {
    pub fn new<S>(target: &str, deps: Vec::<S>, cmd: RobCommand) -> Job
    where
        S: Into::<String>
    {
        let target = target.to_owned();
        let deps = deps.into_iter().map(Into::into).collect::<Vec::<_>>();
        Job {target, deps, cmd, ..Self::default()}
    }

    #[inline(always)]
    pub fn target(&self) -> &String {
        &self.target
    }

    #[inline(always)]
    pub fn deps(&self) -> &Vec::<String> {
        &self.deps
    }

    #[inline(always)]
    pub fn cmd(&self) -> &RobCommand {
        &self.cmd
    }

    #[inline(always)]
    pub fn cfg(&self) -> &Config {
        &self.cmd.cfg
    }

    #[inline(always)]
    pub fn cfg_mut(&mut self) -> &mut Config {
        &mut self.cmd.cfg
    }

    #[inline(always)]
    pub fn reusable_cmd(&mut self, reusable_cmd: bool) -> &mut Self {
        self.reusable_cmd = reusable_cmd;
        self
    }

    #[inline(always)]
    pub fn phony(&mut self, phony: bool) -> &mut Self {
        self.phony = phony;
        self
    }

    #[inline(always)]
    pub fn keepgoing(&mut self, keepgoing: bool) -> &mut Self {
        self.cmd.keepgoing(keepgoing);
        self
    }

    #[inline(always)]
    pub fn echo(&mut self, echo: bool) -> &mut Self {
        self.cmd.echo(echo);
        self
    }

    #[inline]
    pub fn needs_rebuild(&self) -> RobResult::<bool> {
        if self.phony {
            Ok(true)
        } else {
            Rob::needs_rebuild_many(&self.target, &self.deps)
        }
    }

    fn execute(&mut self, sync: bool, exit_: bool, check: bool) -> RobResult::<Vec::<Output>> {
        if !check || self.needs_rebuild()? {
            let cmd = if self.reusable_cmd {
                &mut self.cmd.clone()
            } else {
                &mut self.cmd
            };
            if sync {
                return if exit_ {
                    cmd.execute_all_sync()
                } else {
                    cmd.execute_all_sync_dont_exit()
                }
            } else {
                return if exit_ {
                    cmd.execute_all_async_and_wait()
                } else {
                    cmd.execute_all_async_and_wait_dont_exit()
                }
            }
        } else {
            log!(INFO, "Nothing to be done for '{target}'.", target = self.target);
            Ok(Vec::new())
        }
    }

    #[inline(always)]
    pub fn execute_async_dont_exit(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(false, false, true)
    }

    #[inline(always)]
    pub fn execute_async(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(false, true, true)
    }

    #[inline(always)]
    pub fn execute_sync_dont_exit(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(true, false, true)
    }

    #[inline(always)]
    pub fn execute_sync(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(true, true, true)
    }

    #[inline(always)]
    pub fn execute_async_dont_exit_unchecked(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(false, false, false)
    }

    #[inline(always)]
    pub fn execute_async_unchecked(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(false, true, false)
    }

    #[inline(always)]
    pub fn execute_sync_dont_exit_unchecked(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(true, false, false)
    }

    #[inline(always)]
    pub fn execute_sync_unchecked(&mut self) -> RobResult::<Vec::<Output>> {
        self.execute(true, true, false)
    }
}

/// The main `Rob` structure.
#[derive(Hash, Eq, Debug, Clone, Default, PartialEq)]
pub struct Rob {
    pub cfg: Config,
    cmd: RobCommand,
    jobs: Vec::<Job>,
}

impl Rob {
    pub const MAX_DIR_LVL: usize = 4;

    pub fn new() -> Rob {
        Rob::default()
    }

    /// Checks if src file needs rebuild
    pub fn needs_rebuild(bin: &str, src: &str) -> IoResult::<bool> {
        if !Rob::path_exists(bin) { return Ok(true) }
        let bin_mod_time = Rob::get_last_modification_time(bin).map_err(|err| {
            log!(ERROR, "{err}: {bin}"); err
        })?;
        let src_mod_time = Rob::get_last_modification_time(src).map_err(|err| {
            log!(ERROR, "{err}: {bin}"); err
        })?;
        Ok(src_mod_time > bin_mod_time)
    }

    pub fn needs_rebuild_many(bin: &str, srcs: &Vec::<String>) -> RobResult::<bool> {
        // I collect times on purpose to check if all of the src files exist,
        // to catch unexisting dependencies at `compile time` whether bin path exists or not.

        let mut times = Vec::new();
        for src in srcs.iter() {
            match Rob::get_last_modification_time(src) {
                Ok(time) => times.push(time),
                Err(_) => return Err(RobError::NotFound(src.to_owned()))
            }
        }

        if !Rob::path_exists(bin) { return Ok(true) }

        let bin_mod_time = match Rob::get_last_modification_time(bin) {
            Ok(time) => time,
            Err(_) => return Err(RobError::NotFound(bin.to_owned()))
        };

        Ok(times.into_iter().any(|src_mod_time| src_mod_time > bin_mod_time))
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
    pub fn go_rebuild_yourself(source_path: &str) -> IoResult::<()> {
        let args = env::args().collect::<Vec::<_>>();
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
                .append(&args)
                .execute_sync()
            {
                Ok(_) => {
                    log!(INFO, "Removing: {old_bin_path}");
                    Rob::rm_if_exists(old_bin_path);
                    exit(0);
                }
                Err(err) => {
                    log!(ERROR, "Failed to restart rob from file: {binary_path}: {err}");
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

    /// Takes path and returns it without the file extension
    #[inline]
    pub fn noext(p: &str) -> String {
        p.chars().take_while(|x| *x != '.').collect()
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

    #[track_caller]
    pub fn log(lvl: LogLevel, out: &str) {
        use LogLevel::*;
        match lvl {
            PANIC => panic!("{lvl} {out}"),
            _     => println!("{lvl} {out}")
        }
    }

    #[inline]
    pub fn echo(&mut self, echo: bool) -> &mut Self {
        self.cfg.echo(echo);
        self
    }

    #[inline]
    pub fn keepgoing(&mut self, keepgoing: bool) -> &mut Self {
        self.cfg.keepgoing(keepgoing);
        self
    }

    pub fn append<S>(&mut self, xs: &[S]) -> &mut Self
    where
        S: ToString + PartialEq::<&'static str>
    {
        self.cmd.append(xs);
        self
    }

    pub fn append_mv<S>(&mut self, xs: &[S]) -> &mut Self
    where
        S: ToString + PartialEq::<&'static str>
    {
        self.cmd.append(xs);
        self.cmd.move_acp_ptr();
        self
    }

    fn execute_jobs(&mut self, sync: bool) -> RobResult::<Vec::<Vec::<Output>>> {
        let mut outss = Vec::new();
        for job in self.jobs.iter_mut() {
            let outs = if sync {
                job.execute_sync()
            } else {
                job.execute_async()
            }?;
            outss.push(outs);
        } Ok(outss)
    }

    #[inline]
    pub fn execute_jobs_sync(&mut self) -> RobResult::<Vec::<Vec::<Output>>> {
        self.execute_jobs(true)
    }

    #[inline]
    pub fn execute_jobs_async(&mut self) -> RobResult::<Vec::<Vec::<Output>>> {
        self.execute_jobs(false)
    }

    #[inline]
    pub fn append_job_job(&mut self, job: Job) -> &mut Self {
        self.jobs.push(job);
        self
    }

    #[inline]
    pub fn append_job<S>(&mut self, target: &str, deps: Vec::<S>, cmd: RobCommand) -> &mut Self
    where
        S: Into::<String>
    {
        let job = Job::new(target, deps, cmd);
        self.jobs.push(job);
        self
    }

    #[inline]
    pub fn execute(&mut self) -> IoResult::<&mut Self> {
        self.cmd.execute()?;
        Ok(self)
    }

    #[inline]
    pub fn execute_sync(&mut self) -> IoResult::<Output> {
        self.cmd.execute_sync()
    }

    #[inline]
    pub fn execute_all_sync(&mut self) -> RobResult::<Vec::<Output>> {
        self.cmd.execute_all_sync()
    }

    #[inline]
    pub fn output(&mut self) -> Option::<Output> {
        self.cmd.output()
    }

    #[inline]
    pub fn outputs_refs(&self) -> VecDeque::<&Output> {
        self.cmd.outputs_refs()
    }

    #[inline]
    pub fn outputs(self) -> VecDeque::<Output> {
        self.cmd.outputs()
    }
}

/*
More important TODOs:
    (#2): Parse env::args and do something with them

    (#3): Rob clean feature

Less important TODOs:
    README;
    Examples;
    Other Nob features;
    Documentation;
*/
