use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("USAGE: {prog} <input>", prog = args[0]);
        return Ok(())
    }

    let file_path = &args[1];
    let program = Mm::load_program_from_file(file_path)?;

    let mut vm = Mm::new(&program);

    let mut i = 300;
    while i > 0 && !vm.halt() {
        vm.execute().unwrap();
        i -= 1;
    }
    Ok(println!("{vm}"))
}
