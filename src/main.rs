mod parser;

use std::process::ExitCode;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("Excatly one argument is expected, an expression to evaluate");
        return ExitCode::FAILURE;
    }
    let source = &args[1];
    match parser::parse_expr(&source) {
        parser::ParseRes::NoParse => {
            eprintln!("Could not parse successfully");
            ExitCode::FAILURE
        }
        parser::ParseRes::Parsed { val, .. } => {
            println!("{val}");
            ExitCode::SUCCESS
        }
    }

}
