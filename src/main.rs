#[macro_use]
mod parser;

mod example;
mod example_macro;
mod property;

use std::process::ExitCode;

use clap::{command, Parser, ValueEnum};

use crate::parser::{ParseRes, Session};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    source: String,
    #[arg(long, value_enum, default_value_t = ParserMode::Macro)]
    parser: ParserMode,
    #[arg(long)]
    show_cache: bool,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum ParserMode {
    /// Use parser generated by the peg! macro.
    Macro,
    /// Use hand written parser using parse! macro.
    Functional,
}

fn main() -> ExitCode {
    let args = Args::parse();
    let parse_fn: Box<dyn Fn(&Vec<char>) -> ParseRes<isize>> = match args.parser {
        ParserMode::Macro => Box::new(make_parser(
            example_macro::DerivationTest::parse_with_session,
            args.show_cache,
        )),
        ParserMode::Functional => {
            Box::new(make_parser(example::parse_with_session, args.show_cache))
        }
    };
    let source: Vec<char> = args.source.chars().collect();
    match parse_fn(&source) {
        parser::ParseRes::Parsed { val, .. } => {
            println!("{val}");
            ExitCode::SUCCESS
        }
        parser::ParseRes::NoParse => {
            eprintln!("Could not parse successfully");
            ExitCode::FAILURE
        }
    }
}

fn make_session<D: Default + Clone>(source: &Vec<char>) -> Session<char, D> {
    let cache = vec![D::default(); source.len()];
    parser::Session {
        source: &source,
        cache,
    }
}

fn make_parser<D: Default + Clone + std::fmt::Debug, T>(
    parser: impl Fn(&mut parser::Session<char, D>) -> ParseRes<T>,
    show_cache: bool,
) -> impl Fn(&Vec<char>) -> ParseRes<T> {
    move |source: &Vec<char>| {
        let mut sess = make_session(&source);
        let res = parser(&mut sess);
        if show_cache {
            println!("Session's cache after is {:#?}", sess.cache);
        }
        res
    }
}
