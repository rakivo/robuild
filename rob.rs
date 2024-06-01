use std::{
    fs::{
        create_dir_all,
        read_dir,
        remove_dir_all,
        remove_file
    },
    io::ErrorKind,
    path::PathBuf,
    collections::VecDeque,
    thread::{self, JoinHandle},
    process::{Command, Output, Stdio}
};

const CMD_ARG: &str = if cfg!(windows) {"cmd"} else {"sh"};
const CMD_ARG2: &str = if cfg!(windows) {"/C"} else {"-c"};

macro_rules! colored {
    (r.$str: expr)  => { format!("\x1b[91m{}\x1b[0m", $str) };
    (y.$str: expr)  => { format!("\x1b[93m{}\x1b[0m", $str) };
    (br.$str: expr) => { format!("\x1b[31m{}\x1b[0m", $str) };
}

#[macro_export]
macro_rules! log {
    ($log_level: expr, $($args: expr), *) => {{
        let out = format!($($args), *);
        match $log_level {
            LogLevel::PANIC => {
                let out = format!("{lvl} {f}:{l}:{c}: {out}",
                                  lvl = $log_level, f = file!(),
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
    /// if current element is file it returns it,
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
        use LogLevel::*;
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
    pub fn mkdir<P>(p: P) -> std::io::Result<()>
    where
        P: Into::<PathBuf>
    {
        create_dir_all(p.into())
    }

    pub fn rm<P>(p: P) -> std::io::Result<()>
    where
        P: Into::<PathBuf> + Clone
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
        use LogLevel::*;
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
    /// Each of the line will be executed as LINES, like you using cmd, for example:
    /// ```
    /// let p = pathbuf!("dummy", "rakivo", "dummy.cpp");
    /// let handle = Rob::new()
    ///     .append(&["clang++", "-o", "output", p.to_str().unwrap()])
    ///     .execute_sync();
    /// ```
    /// Outputs:
    /// [CMD] clang++ -o output test/test1/test.cpp
    /// [INFO] [EMPTY]
    ///
    /// Output is [EMPTY] because the program compiled successfully.
    #[inline]
    pub fn append(&mut self, args: &[&str]) -> &mut Self {
        self.0.lines.push(args.join(" "));
        self
    }

    fn format_out(out: &str) -> &str {
        if out.is_empty() {
            "[EMPTY]"
        } else if out.ends_with('\n') {
            &out[0..out.len() - 1]
        } else {
            out
        }
    }

    fn process_output(out: &Output) {
        if out.status.success() {
            let stdout = String::from_utf8_lossy(&out.stdout);
            let formatted = Rob::format_out(&stdout);
            log!(LogLevel::INFO, "{formatted}");
        } else {
            let stderr = String::from_utf8_lossy(&out.stderr);
            let formatted = Rob::format_out(&stderr);
            log!(LogLevel::ERROR, "{formatted}");
        }
    }

    /// Blocking operation.
    ///
    /// Simply creates `Command` for each line in self.lines and executes them.
    /// More about that: https://doc.rust-lang.org/std/process/struct.Command.html
    pub fn execute_sync(&mut self) -> Vec::<Result::<Output, ()>> {
        self.0.lines.iter().map(|line| {
            log!(LogLevel::CMD, "{line}");
            let out = Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(&line)
                .output()
                .map_err(|err| log!(LogLevel::ERROR, "{err}"))?;

            Self::process_output(&out);

            Ok(out)
        }).collect()
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
    pub fn execute_async(&mut self) -> JoinHandle::<Vec::<std::io::Result::<Output>>> {
        let children = self.0.lines.iter().map(|line| {
            log!(LogLevel::CMD, "{line}");
            Command::new(CMD_ARG)
                .arg(CMD_ARG2)
                .arg(&line)
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .map_err(|err| {
                    log!(LogLevel::PANIC, "FAILED TO EXECUTE PROCESS: {err}")
                }).unwrap()
        }).collect::<Vec<_>>();

        thread::spawn(move || {
            children.into_iter().map(|child| {
                child.wait_with_output().map_err(|err| {
                    log!(LogLevel::PANIC, "FAILED TO WAIT FOR CHILD: {err}");
                    err
                })
            }).collect()
        })
    }

    /// Blocks the main thread and waits for all of the children.
    pub fn wait_for_children(handle: JoinHandle::<Vec::<std::io::Result::<Output>>>) {
        handle.join()
            .unwrap()
            .iter()
            .filter_map(|res| res.as_ref().ok())
            .for_each(|out| {
                Rob::process_output(&out);
            });
    }
}

/* TODO:
    Examples,
    Other Nob features,
    Documentation,
*/
