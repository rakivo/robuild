use std::env;

extern crate mm;
use mm::*;

fn main() -> std::io::Result<()> {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 3 {
        eprintln!("USAGE: {prog} <input> <output>", prog = args[0]);
        return Ok(())
    }

    let input_file_path = &args[1];
    let output_file_path = &args[2];
    let translated_program = Mm::translate_masm(&input_file_path)?;
    Mm::save_program_to_file(&translated_program, &output_file_path)
}
