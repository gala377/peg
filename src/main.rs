mod parser;

use std::process::ExitCode;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    match &args[..] {
        [_, flag, source] if flag == "--use_macro" => match parser::DerivationTest::parse(source) {
            parser::ParseRes::NoParse => {
                eprintln!("Could not parse successfully");
                ExitCode::FAILURE
            }
            parser::ParseRes::Parsed { val, .. } => {
                println!("{val}");
                ExitCode::SUCCESS
            }
        },
        [_, source] => match parser::parse_expr(&source) {
            parser::ParseRes::NoParse => {
                eprintln!("Could not parse successfully");
                ExitCode::FAILURE
            }
            parser::ParseRes::Parsed { val, .. } => {
                println!("{val}");
                ExitCode::SUCCESS
            }
        },
        _ => {
            eprintln!("Expected an expression to parse with an optional flag at best");
            ExitCode::FAILURE
        }
    }
}
