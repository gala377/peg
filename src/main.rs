#[macro_use]
mod parser;

mod example;
mod example_macro;
mod property;

use std::process::ExitCode;

fn main() -> ExitCode {
    let args = std::env::args().collect::<Vec<_>>();
    match &args[..] {
        [_, flag, source] if flag == "--use_macro" => {
            let source: Vec<char> = source.chars().collect();
            let cache = vec![example_macro::DerivationTest::default(); source.len()];
            let mut sess = parser::Session {
                source: &source,
                cache,
            };
            match example_macro::DerivationTest::parse_with_session(&mut sess) {
                parser::ParseRes::NoParse => {
                    eprintln!("Could not parse successfully");
                    ExitCode::FAILURE
                }
                parser::ParseRes::Parsed { val, .. } => {
                    println!("{val}");
                    println!("Session's cache after is {:#?}", sess.cache);
                    ExitCode::SUCCESS
                    // ((1+1+((10)+1)))
                }
            }
        }
        [_, source] => match example::parse_expr(&source) {
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
