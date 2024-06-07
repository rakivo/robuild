use std::fs::read_to_string;
use robuild::*;

const LIB_FLAGS: &str = "--crate-type=rlib";
const DEBUG_FLAGS: &str = "-g -C \"opt-level=0\"";
const THREAD_FLAGS: &str = "-Z threads=10";
const LINK_WITH_MM_FLAGS: &str = if cfg!(windows) {
    "--extern mm=build\\libmm.lib"
} else {
    "--extern mm=build/libmm.rlib"
};

fn main() -> IoResult::<()> {
    go_rebuild_yourself!(?);

    Rob::mkdir("build").unwrap();

    let src = path!("src", "main.c");
    let out = path!("build", "main");
    let mut cmd = RobCommand::new();
    cmd.append(&[CC, "-o", &out, &src]);

    let foo_src = path!("src", "foo.c");
    let foo_out = path!("build", "foo");
    let mut foo_cmd = RobCommand::new();
    foo_cmd
        .append_mv(&[CC, "-c", &path!("src" ,"hello.c"), "-o", &path!("build", "hello.o")])
        .append_mv(&["ar ruv", "libhello.a", &path!("build", "hello.o")])
        .append(&[CC, "-o", &foo_out, &foo_src, "libhello.a"]);

    let mut mm_cmd = RobCommand::new();
    let mm_src = path!("mm", "mm.rs");
    let mm_out = path!("build", "libmm.rlib");
    mm_cmd
        .append(&["rustc", DEBUG_FLAGS, LIB_FLAGS, THREAD_FLAGS, "-o"])
        .append_mv(&[&path!("build", "libmm.rlib"), &mm_src]);

    let mut load_from_bin_cmd = RobCommand::new();
    let load_from_bin_src = path!("mm", "examples", "load_from_binary.rs");
    let load_from_bin_out = path!("build", "load_from_binary");
    load_from_bin_cmd
        .append(&["rustc", DEBUG_FLAGS, THREAD_FLAGS, LINK_WITH_MM_FLAGS, "-o"])
        .append_mv(&[&load_from_bin_out, &load_from_bin_src]);

    let mut translate_masm_cmd = RobCommand::new();
    let translate_masm_src = path!("mm", "examples", "translate_masm.rs");
    let translate_masm_out = path!("build", "translate_masm");
    translate_masm_cmd
        .append(&["rustc", DEBUG_FLAGS, THREAD_FLAGS, LINK_WITH_MM_FLAGS, "-o"])
        .append_mv(&[&translate_masm_out, &translate_masm_src]);

    let test_mm_out = path!("build", "fibm.out");
    let test_mm_expected = path!("mm", "load_from_binary.expected");

    let mut test_mm_cmd = RobCommand::new();
    test_mm_cmd
       .append(&[&path!("build", "translate_masm")])
       .append_mv(&[&path!("mm", "fib.masm"), &path!("build", "fibm")])
       .append(&[&path!("build", "load_from_binary")])
       .append(&[&format!("{p1} > {p2}", p1 = &path!("build", "fibm"), p2 = &test_mm_out)]);

    Rob::new()
        .append_job(Job::new(Some(&out), vec![&src, &path!("src", "main.h")], cmd))
        .append_job(Job::new(Some(&foo_out), vec![&foo_src, &path!("src", "hello.c")], foo_cmd))
        .append_job(Job::new(Some(&mm_out), vec![&mm_src], mm_cmd))
        .append_job(Job::new(Some(&translate_masm_out), vec![&load_from_bin_src, &mm_src], translate_masm_cmd))
        .append_job(Job::new(Some(&load_from_bin_out), vec![&translate_masm_src, &mm_src], load_from_bin_cmd))
        .append_job(Job::new(None, vec![&load_from_bin_src, &translate_masm_src], test_mm_cmd))
        .execute_jobs_async()?;

    let output_string = read_to_string(&test_mm_out)?;
    let expected_string = read_to_string(&test_mm_expected)?;
    if output_string.trim() != expected_string.trim() {
        log!(PANIC, "Output of {test_mm_out} doesn't equal to the expected one: {test_mm_expected}");
    } else {
        log!(INFO, "TEST: `translate_masm`: OK");
    }

    Ok(())
}
