/*

grammar:

    start:          S <- A
    addition:       A <- M + A | M
    multiplication: M <- T * M | T
    term:           T <- ((A)) | (A) | N
    number:         N <- 0 | [1-9][0-9]*

((A)) is a special double paren that doubles the value inside
*/

use std::fmt::Write;

#[derive(Clone, Debug)]
pub enum ParseRes<T> {
    Parsed { val: T, next_pos: usize },
    NoParse,
}

#[derive(Default, Clone)]
struct Derivation {
    addition: Option<ParseRes<isize>>,
    term: Option<ParseRes<isize>>,
    double_parenthesis: Option<ParseRes<isize>>,
    parenthesis: Option<ParseRes<isize>>,
    number: Option<ParseRes<isize>>,
}

// Each index corresponds to the position in the input
// So `parser_cache[0].adddition` is a result of calling addition parsing function
// when no tokens have been eaten yet.
type ParserCache<D> = Vec<D>;

trait Token: Clone {
    const EOF: Self;
}

impl Token for char {
    const EOF: Self = 0x05 as char;
}

#[derive(Clone)]
struct Session<'input, T: Token, D> {
    source: &'input Vec<T>,
    cache: ParserCache<D>,
}

trait CacheOp<D, T> {
    fn extract(&self, d: &D) -> Option<ParseRes<T>>;
    fn update(&self, d: &mut D, v: ParseRes<T>);
}

impl<T, ExtractFn, UpdateFn> CacheOp<Derivation, T> for (ExtractFn, UpdateFn)
where
    ExtractFn: Fn(&Derivation) -> Option<ParseRes<T>>,
    UpdateFn: Fn(&mut Derivation, ParseRes<T>),
{
    fn extract(&self, d: &Derivation) -> Option<ParseRes<T>> {
        (self.0)(d)
    }

    fn update(&self, d: &mut Derivation, v: ParseRes<T>) {
        (self.1)(d, v)
    }
}

macro_rules! field {
    ($name:ident) => {
        (
            |d: &Derivation| d.$name.clone(),
            |d: &mut Derivation, v: ParseRes<_>| d.$name = Some(v),
        )
    };
}

fn parse<R: Clone, T: Token, D>(
    sess: &mut Session<T, D>,
    pos: usize,
    cache_op: impl CacheOp<D, R>,
    parser: impl Fn(&mut Session<T, D>, usize) -> ParseRes<R>,
) -> ParseRes<R> {
    eprintln!("parse[{pos}]: cache lookup");
    let cache = &mut sess.cache;
    let lookup = cache_op.extract(&cache[pos]);
    match lookup {
        Some(res) => res,
        None => {
            eprintln!("parse[{pos}]: nothing in cache");
            let res = parser(sess, pos);
            cache_op.update(&mut sess.cache[pos], res.clone());
            res
        }
    }
}

type ParseSess<'a> = Session<'a, char, Derivation>;

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

fn curr_tok<T: Token, D>(sess: &Session<T, D>, pos: usize) -> T {
    match sess.source.get(pos) {
        None => T::EOF,
        Some(tok) => tok.clone(),
    }
}

fn eat<T: Token, D>(sess: &Session<T, D>, pos: usize) -> (usize, T) {
    (pos + 1, curr_tok(sess, pos))
}

/*

Macro rules implementation adding eDSL for defining PEG parsers.

*/

macro_rules! field_op {
    ($tname:ident.$fname:ident) => {
        (
            |d: &$tname| d.$fname.clone(),
            |d: &mut $tname, v: ParseRes<_>| d.$fname = Some(v),
        )
    };
}

macro_rules! parse {
    ($sess:expr, $pos:expr, $fname:ident) => {
        parse($sess, $pos, field_op!(Self.$fname), Self::$fname)
    };

    ($sess:expr, $pos:expr, $tname:ident::$fname:ident) => {
        parse($sess, $pos, field_op!($tname.$fname), $tname::$fname)
    };
}

macro_rules! peg {
    (grammar $name:ident { $($rule:ident($sess:pat, $pos:pat) -> $t:ty $body:block)+ }) => {

        impl<T, ExtractFn, UpdateFn> CacheOp<$name, T> for (ExtractFn, UpdateFn)
        where
            ExtractFn: Fn(&$name) -> Option<ParseRes<T>>,
            UpdateFn: Fn(&mut $name, ParseRes<T>),
        {
            fn extract(&self, d: &$name) -> Option<ParseRes<T>> {
                (self.0)(d)
            }

            fn update(&self, d: &mut $name, v: ParseRes<T>) {
                (self.1)(d, v)
            }
        }


        #[derive(Default, Clone, Debug)]
        pub struct $name {
            $(
                $rule: Option<ParseRes<$t>>
            ),+
        }
        impl $name {
            $(
                fn $rule(sess: &mut Session<char, $name>, pos: usize) -> ParseRes<$t> {
                    let f = |$sess: &mut Session<char, $name>, $pos: usize| -> ParseRes<$t> { $body };
                    parse(
                        sess,
                        pos,
                        field_op!($name.$rule),
                        f)
                }
            )+
        }
    };
}

peg! {
grammar DerivationTest {
    start(sess, pos) -> isize {
        parse!(sess, pos, addition)
    }

    addition(sess, pos) -> isize {
        fn just_term(sess: &mut Session<char, DerivationTest>, pos: usize) -> ParseRes<isize> {
            eprintln!("parse_addition[({pos})]: just term");
            parse!(sess, pos, DerivationTest::term)
        }

        eprintln!("parse_addition[({pos})]: enter");
        let old_pos = pos;
        match parse!(sess, pos, term) {
            ParseRes::Parsed {
                val: left,
                next_pos: mut pos,
            } => {
                if curr_tok(sess, pos) != '+' {
                    eprintln!("parse_addition[({pos})]: expected a +");
                    return just_term(sess, old_pos);
                }
                (pos, _) = eat(sess, pos);
                match parse!(sess, pos, addition) {
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

    term(sess, pos) -> isize {
        eprintln!("parse_term[{pos}]: parsing double paren");
        if let res @ ParseRes::Parsed { .. } =
            parse!(sess, pos, double_paren)
        {
            eprintln!("parse_term[{pos}]: got double paren");
            return res;
        }
        eprintln!("parse_term[{pos}]: parse parenthesis");
        if let res @ ParseRes::Parsed { .. } = parse!(sess, pos, paren) {
            eprintln!("parse_term[{pos}]: got parenthesis");
            return res;
        }
        eprintln!("parse_term[{pos}]: parsing a number");
        parse!(sess, pos, number)
    }

    double_paren(sess, mut pos) -> isize {
        eprintln!("parse_double_paren[{pos}]: enter");
        if curr_tok(sess, pos) != '(' {
            eprintln!("parse_double_paren[{pos}]: not a lparen");
            return ParseRes::NoParse;
        }
        (pos, _) = eat(sess, pos);
        eprintln!("parse_double_paren[{pos}]: call parse paren");
        match parse!(sess, pos, paren) {
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

    paren(sess, mut pos) -> isize {
        eprintln!("parse_paren[{pos}]: enter");
        if curr_tok(sess, pos) != '(' {
            eprintln!("parse_paren[{pos}]: not a lparen");
            return ParseRes::NoParse;
        }
        (pos, _) = eat(sess, pos);
        eprintln!("parse_paren[{pos}]: call parse term");
        match parse!(sess, pos, addition) {
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
    fn test_cache_op_making_macro() {
        let mut d = Derivation {
            addition: Some(ParseRes::NoParse),
            ..Default::default()
        };
        let op = field!(addition);
        assert_matches!(op.extract(&d), Some(ParseRes::NoParse));
        let expected = ParseRes::Parsed {
            val: 10,
            next_pos: 1,
        };
        op.update(&mut d, expected.clone());
        assert_matches!(
            op.extract(&d),
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

    #[test]
    fn deriv_parse_expr_parses_zero() {
        assert_matches!(
            DerivationTest::parse("0"),
            ParseRes::Parsed {
                val: 0,
                next_pos: 1
            }
        );
    }

    #[test]
    fn deriv_parse_expr_parses_longer_number() {
        assert_matches!(
            DerivationTest::parse("123456"),
            ParseRes::Parsed {
                val: 123456,
                next_pos: 6
            }
        );
    }

    #[test]
    fn deriv_parse_expr_parses_number_in_parenthesis() {
        assert_matches!(
            DerivationTest::parse("(123)"),
            ParseRes::Parsed {
                val: 123,
                next_pos: 5
            }
        );
    }

    #[test]
    fn deriv_parse_expr_doubles_value_in_double_paren() {
        assert_matches!(
            DerivationTest::parse("((20))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 6
            }
        );
    }
    #[test]
    fn deriv_parse_expr_doubles_value_in_double_paren_inside_paren() {
        assert_matches!(
            DerivationTest::parse("(((20)))"),
            ParseRes::Parsed {
                val: 40,
                next_pos: 8
            }
        );
    }
    #[test]
    fn deriv_parse_expr_quadruples_value_in_double_double_paren() {
        assert_matches!(
            DerivationTest::parse("((((20))))"),
            ParseRes::Parsed {
                val: 80,
                next_pos: 10
            }
        );
    }

    #[test]
    fn deriv_parse_expr_parses_addition() {
        assert_matches!(
            DerivationTest::parse("1+2"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 3
            }
        );
    }

    #[test]
    fn deriv_parse_expr_parses_double_parenthesis_meaning_that_do_not_double() {
        assert_matches!(
            DerivationTest::parse("((1+1)+1)"),
            ParseRes::Parsed {
                val: 3,
                next_pos: 9
            }
        );
    }

    #[test]
    fn deriv_parse_expr_does_not_match_double_paren_greedily() {
        assert_matches!(
            DerivationTest::parse("(((1+1))+1)"),
            ParseRes::Parsed {
                val: 5,
                next_pos: 11
            }
        );
    }
}
