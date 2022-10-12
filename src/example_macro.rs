use std::fmt::Write;

use crate::parser::{curr_tok, eat, parse, ParseRes, Session};

/*
grammar:

    start:          S <- A
    addition:       A <- M + A | M
    multiplication: M <- T * M | T
    term:           T <- ((A)) | (A) | N
    number:         N <- 0 | [1-9][0-9]*

((A)) is a special double paren that doubles the value inside
*/

peg! {
grammar DerivationTest {
    start(sess, pos) -> isize {
        Self::addition(sess, pos)
    }

    addition(sess, pos) -> isize {
        eprintln!("parse_addition[({pos})]: enter");
        let old_pos = pos;
        match Self::term(sess, pos) {
            ParseRes::Parsed {
                val: left,
                next_pos: mut pos,
            } if curr_tok(sess, pos) == '+' => {
                (pos, _) = eat(sess, pos);
                match Self::addition(sess, pos) {
                    ParseRes::NoParse => {
                        eprintln!("parse_addition[({pos})]: left side is not a term");
                        Self::term(sess, old_pos)
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
            ParseRes::Parsed { .. } => {
                eprintln!("parse_addition[({pos})]: expected a +");
                Self::term(sess, old_pos)
            }
            ParseRes::NoParse => Self::term(sess, old_pos),
        }
    }

    term(sess, pos) -> isize {
        or!(sess, pos, double_paren, paren, number)
    }

    double_paren(sess, mut pos) -> isize {
        eprintln!("parse_double_paren[{pos}]: enter");
        if curr_tok(sess, pos) != '(' {
            eprintln!("parse_double_paren[{pos}]: not a lparen");
            return ParseRes::NoParse;
        }
        (pos, _) = eat(sess, pos);
        eprintln!("parse_double_paren[{pos}]: call parse paren");
        match Self::paren(sess, pos) {
            ParseRes::NoParse => ParseRes::NoParse,
            ParseRes::Parsed {
                val,
                next_pos: mut pos,
            } if curr_tok(sess, pos) == ')' => {
                (pos, _) = eat(sess, pos);
                ParseRes::Parsed {
                    val: val * 2,
                    next_pos: pos,
                }
            }
            ParseRes::Parsed { .. } => {
                eprintln!("parse_double_paren[{pos}]: expected rparen");
                ParseRes::NoParse
            }
        }
    }

    paren(sess, mut pos) -> isize {
        eprintln!("parse_paren[{pos}]: enter");
        if curr_tok(sess, pos) != '(' {
            eprintln!("parse_paren[{pos}]: not a lparen");
            return ParseRes::NoParse;
        }
        (pos, _) = eat(sess, pos);
        eprintln!("parse_paren[{pos}]: call parse addition");
        match Self::addition(sess, pos) {
            ParseRes::NoParse => ParseRes::NoParse,
            ParseRes::Parsed {
                val,
                next_pos: mut pos,
            } if curr_tok(sess, pos) == ')' => {
                (pos, _) = eat(sess, pos);
                ParseRes::Parsed { val, next_pos: pos }
            }
            ParseRes::Parsed { .. } => {
                eprintln!("parse_paren[{pos}]: expected rparen");
                ParseRes::NoParse
            }
        }
    }

    number(sess, mut pos) -> isize {
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
}
}

impl DerivationTest {
    pub fn parse(source: &str) -> ParseRes<isize> {
        let source: Vec<char> = source.chars().collect();
        let cache = vec![DerivationTest::default(); source.len()];
        let mut sess = Session {
            source: &source,
            cache,
        };
        eprintln!("parse_expr[0]: call parse_addition");
        DerivationTest::start(&mut sess, 0)
    }

    pub fn parse_with_session(sess: &mut Session<char, Self>) -> ParseRes<isize> {
        DerivationTest::start(sess, 0)
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

    use super::*;

    #[test]
    fn parse_expr_parses_zero() {
        assert_matches!(
            DerivationTest::parse("0"),
            ParseRes::Parsed {
                val: 0,
                next_pos: 1
            }
        );
    }

    #[test]
    fn parse_expr_parses_longer_number() {
        assert_matches!(
            DerivationTest::parse("123456"),
            ParseRes::Parsed {
                val: 123456,
                next_pos: 6
            }
        );
    }

    #[test]
    fn parse_expr_parses_number_in_parenthesis() {
        assert_matches!(
            DerivationTest::parse("(123)"),
            ParseRes::Parsed {
                val: 123,
                next_pos: 5
            }
        );
    }

    #[test]
    fn parse_expr_doubles_value_in_double_paren() {
        assert_matches!(
            DerivationTest::parse("((20))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 6
            }
        );
    }
    #[test]
    fn parse_expr_doubles_value_in_double_paren_inside_paren() {
        assert_matches!(
            DerivationTest::parse("(((20)))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 8
            }
        );
    }
    #[test]
    fn parse_expr_quadruples_value_in_double_double_paren() {
        assert_matches!(
            DerivationTest::parse("((((20))))"),
            ParseRes::Parsed {
                val: 80,
                next_pos: 10
            }
        );
    }

    #[test]
    fn parse_expr_parses_addition() {
        assert_matches!(
            DerivationTest::parse("1+2"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 3
            }
        );
    }

    #[test]
    fn parse_expr_parses_double_parenthesis_meaning_that_do_not_double() {
        assert_matches!(
            DerivationTest::parse("((1+1)+1)"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 9
            }
        );
    }

    #[test]
    fn parse_expr_does_not_match_double_paren_greedily() {
        assert_matches!(
            DerivationTest::parse("(((1+1))+1)"),
            ParseRes::Parsed {
                val: 5,
                next_pos: 11
            }
        );
    }
}
