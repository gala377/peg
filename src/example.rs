use std::fmt::Write;

use crate::parser::{curr_tok, eat, parse, Session, ParseRes};

/*
grammar:

    start:          S <- A
    addition:       A <- M + A | M
    multiplication: M <- T * M | T
    term:           T <- ((A)) | (A) | N
    number:         N <- 0 | [1-9][0-9]*

((A)) is a special double paren that doubles the value inside
*/

#[derive(Default, Clone)]
struct Derivation {
    addition: Option<ParseRes<isize>>,
    term: Option<ParseRes<isize>>,
    double_parenthesis: Option<ParseRes<isize>>,
    parenthesis: Option<ParseRes<isize>>,
    number: Option<ParseRes<isize>>,
}

type ParseSess<'a> = Session<'a, char, Derivation>;

macro_rules! field {
    ($name:ident) => {
        (
            |d: &Derivation| d.$name.clone(),
            |d: &mut Derivation, v: ParseRes<_>| d.$name = Some(v),
        )
    };
}

pub fn parse_expr(source: &str) -> ParseRes<isize> {
    let source: Vec<char> = source.chars().collect();
    let cache = vec![Derivation::default(); source.len()];
    let mut sess = Session {
        source: &source,
        cache,
    };
    eprintln!("parse_expr[0]: call parse_addition");
    match parse(&mut sess, 0, field!(addition), parse_addition) {
        ParseRes::Parsed { next_pos, .. } if next_pos < source.len() => {
            eprintln!(
                "Not all input has been eaten: {} chars left",
                source.len() - next_pos
            );
            ParseRes::NoParse
        }
        res @ _ => res,
    }
}

fn parse_addition(sess: &mut ParseSess, pos: usize) -> ParseRes<isize> {
    fn just_term(sess: &mut ParseSess, pos: usize) -> ParseRes<isize> {
        eprintln!("parse_addition[({pos})]: just term");
        parse(sess, pos, field!(term), parse_term)
    }

    eprintln!("parse_addition[({pos})]: enter");
    let old_pos = pos;
    match parse(sess, pos, field!(term), parse_term) {
        ParseRes::Parsed {
            val: left,
            next_pos: mut pos,
        } => {
            if curr_tok(sess, pos) != '+' {
                eprintln!("parse_addition[({pos})]: expected a +");
                return just_term(sess, old_pos);
            }
            (pos, _) = eat(sess, pos);
            match parse(sess, pos, field!(addition), parse_addition) {
                ParseRes::NoParse => {
                    eprintln!("parse_addition[({pos})]: left side is not a term");
                    just_term(sess, old_pos)
                }
                ParseRes::Parsed {
                    val: right,
                    next_pos,
                } => ParseRes::Parsed {
                    val: left + right,
                    next_pos,
                },
            }
        }
        ParseRes::NoParse => just_term(sess, old_pos),
    }
}

fn parse_term(sess: &mut ParseSess, pos: usize) -> ParseRes<isize> {
    eprintln!("parse_term[{pos}]: parsing double paren");
    if let res @ ParseRes::Parsed { .. } =
        parse(sess, pos, field!(double_parenthesis), parse_double_paren)
    {
        eprintln!("parse_term[{pos}]: got double paren");
        return res;
    }
    eprintln!("parse_term[{pos}]: parse parenthesis");
    if let res @ ParseRes::Parsed { .. } = parse(sess, pos, field!(parenthesis), parse_paren) {
        eprintln!("parse_term[{pos}]: got parenthesis");
        return res;
    }
    eprintln!("parse_term[{pos}]: parsing a number");
    parse(sess, pos, field!(number), parse_number)
}

fn parse_double_paren(sess: &mut ParseSess, mut pos: usize) -> ParseRes<isize> {
    eprintln!("parse_double_paren[{pos}]: enter");
    if curr_tok(sess, pos) != '(' {
        eprintln!("parse_double_paren[{pos}]: not a lparen");
        return ParseRes::NoParse;
    }
    (pos, _) = eat(sess, pos);
    eprintln!("parse_double_paren[{pos}]: call parse paren");
    match parse(sess, pos, field!(parenthesis), parse_paren) {
        ParseRes::NoParse => ParseRes::NoParse,
        ParseRes::Parsed {
            val,
            next_pos: mut pos,
        } => {
            if curr_tok(sess, pos) != ')' {
                eprintln!("parse_double_paren[{pos}]: expected rparen");
                ParseRes::NoParse
            } else {
                (pos, _) = eat(sess, pos);
                ParseRes::Parsed {
                    val: val * 2,
                    next_pos: pos,
                }
            }
        }
    }
}

fn parse_paren(sess: &mut ParseSess, mut pos: usize) -> ParseRes<isize> {
    eprintln!("parse_paren[{pos}]: enter");
    if curr_tok(sess, pos) != '(' {
        eprintln!("parse_paren[{pos}]: not a lparen");
        return ParseRes::NoParse;
    }
    (pos, _) = eat(sess, pos);
    eprintln!("parse_paren[{pos}]: call parse term");
    match parse(sess, pos, field!(addition), parse_addition) {
        ParseRes::NoParse => ParseRes::NoParse,
        ParseRes::Parsed {
            val,
            next_pos: mut pos,
        } => {
            if curr_tok(sess, pos) != ')' {
                eprintln!("parse_paren[{pos}]: expected rparen");
                ParseRes::NoParse
            } else {
                (pos, _) = eat(sess, pos);
                ParseRes::Parsed { val, next_pos: pos }
            }
        }
    }
}

fn parse_number(sess: &mut ParseSess, mut pos: usize) -> ParseRes<isize> {
    eprintln!("parse_number[{pos}]: enter");
    let tok = curr_tok(sess, pos);
    let (mut buf, is_zero) = match tok {
        '0' => {
            (pos, _) = eat(sess, pos);
            ('0'.to_string(), true)
        }
        _ => (String::new(), false),
    };
    while let ch @ '0'..='9' = curr_tok(sess, pos) {
        (pos, _) = eat(sess, pos);
        buf.write_char(ch).unwrap();
    }
    if is_zero && buf.len() > 1 {
        // here we could emit an error as this is a wrong literal
        // but that is a job for a lexer
        eprintln!("parse_number[{pos}]: the buffer starts with 0: {buf}");
        return ParseRes::NoParse;
    }
    match curr_tok(sess, pos) {
        tok if tok.is_alphabetic() =>
        // Again, wrong literal here
        {
            eprintln!("parse_number[{pos}]: the following literal is {tok}. This is not legal.");
            ParseRes::NoParse
        }
        _ if buf.is_empty() => ParseRes::NoParse,
        _ => ParseRes::Parsed {
            val: buf.parse().unwrap(),
            next_pos: pos,
        },
    }
}

#[cfg(test)]
mod tests {

    macro_rules! assert_matches {
        ($cond:expr, $p:pat) => {
            let v = $cond;
            match v {
                $p => (),
                _ => {
                    assert!(
                        false,
                        "The value {:?} did not match the pattern {}",
                        v,
                        stringify!($p)
                    );
                }
            }
        };
    }

    use crate::property::Property;
    use super::*;

    #[test]
    fn test_cache_op_making_macro() {
        let mut d = Derivation {
            addition: Some(ParseRes::NoParse),
            ..Default::default()
        };
        let op = field!(addition);
        assert_matches!(op.get(&d), Some(ParseRes::NoParse));
        let expected = ParseRes::Parsed {
            val: 10,
            next_pos: 1,
        };
        op.set(&mut d, expected.clone());
        assert_matches!(
            op.get(&d),
            Some(ParseRes::Parsed {
                val: 10,
                next_pos: 1
            })
        );
    }

    #[test]
    fn parse_expr_parses_zero() {
        assert_matches!(
            parse_expr("0"),
            ParseRes::Parsed {
                val: 0,
                next_pos: 1
            }
        );
    }

    #[test]
    fn parse_expr_parses_longer_number() {
        assert_matches!(
            parse_expr("123456"),
            ParseRes::Parsed {
                val: 123456,
                next_pos: 6
            }
        );
    }

    #[test]
    fn parse_expr_parses_number_in_parenthesis() {
        assert_matches!(
            parse_expr("(123)"),
            ParseRes::Parsed {
                val: 123,
                next_pos: 5
            }
        );
    }

    #[test]
    fn parse_expr_doubles_value_in_double_paren() {
        assert_matches!(
            parse_expr("((20))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 6
            }
        );
    }
    #[test]
    fn parse_expr_doubles_value_in_double_paren_inside_paren() {
        assert_matches!(
            parse_expr("(((20)))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 8
            }
        );
    }
    #[test]
    fn parse_expr_quadruples_value_in_double_double_paren() {
        assert_matches!(
            parse_expr("((((20))))"),
            ParseRes::Parsed {
                val: 80,
                next_pos: 10
            }
        );
    }

    #[test]
    fn parse_expr_parses_addition() {
        assert_matches!(
            parse_expr("1+2"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 3
            }
        );
    }

    #[test]
    fn parse_expr_parses_double_parenthesis_meaning_that_do_not_double() {
        assert_matches!(
            parse_expr("((1+1)+1)"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 9
            }
        );
    }

    #[test]
    fn parse_expr_does_not_match_double_paren_greedily() {
        assert_matches!(
            parse_expr("(((1+1))+1)"),
            ParseRes::Parsed {
                val: 5,
                next_pos: 11
            }
        );
    }
}
