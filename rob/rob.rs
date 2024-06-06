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

    let mut rob = Rob::new();
    build_rakivo_mm(&mut rob)?;
    test_rakivo_mm(&mut rob)?;

    Ok(())
}

fn build_file(rob: &mut Rob, out: &str, name: &str, flags: &str) {
    rob.append(&["rustc", DEBUG_FLAGS, flags, THREAD_FLAGS, LINK_WITH_MM_FLAGS, "-o"])
       .append_mv(&[&path!("build", out), &path!("mm", &format!("{name}.rs"))]);
}

// Link to Rakivo's mm: https://github.com/rakivo/mm
fn build_rakivo_mm(rob: &mut Rob) -> IoResult::<()> {
    build_file(rob, "libmm.rlib", "mm", LIB_FLAGS);
    build_file(rob, "load_from_binary", &path!("examples", "load_from_binary"), "");
    build_file(rob, "translate_masm", &path!("examples", "translate_masm"), "");
    rob.execute_all_sync()?;
    Ok(())
}

fn test_rakivo_mm(rob: &mut Rob) -> IoResult::<()> {
    use std::fs::read_to_string;

    let output_path = path!("build", "fibm.out");
    let expected_path = path!("mm", "load_from_binary.expected");

    rob.append(&[&path!("build", "translate_masm")])
       .append_mv(&[&path!("mm", "fib.masm"), &path!("build", "fibm")])
       .append(&[&path!("build", "load_from_binary")])
       .append(&[&format!("{p1} > {p2}", p1 = &path!("build", "fibm"), p2 = &output_path)])
       .execute_all_sync()?;

    let output_string = read_to_string(&output_path)?;
    let expected_string = read_to_string(&expected_path)?;
    if output_string.trim() != expected_string.trim() {
        log!(PANIC, "Output of {output_path} doesn't equal to the expected one: {expected_path}");
    } else {
        log!(INFO, "TEST: `translate_masm`: OK");
    }

    Ok(())
}
