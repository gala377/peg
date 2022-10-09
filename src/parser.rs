#[macro_use]
mod peg_macro;
mod session;
mod token;

use crate::property::Property;
pub use session::Session;
use token::Token;

#[derive(Clone, Debug)]
pub enum ParseRes<T> {
    Parsed { val: T, next_pos: usize },
    NoParse,
}

pub fn parse<Res: Clone, Tok: Token, Derivation>(
    sess: &mut Session<Tok, Derivation>,
    pos: usize,
    cache_op: impl Property<Derivation, Option<ParseRes<Res>>, ParseRes<Res>>,
    parser: impl Fn(&mut Session<Tok, Derivation>, usize) -> ParseRes<Res>,
) -> ParseRes<Res> {
    eprintln!("parse[{pos}]: cache lookup");
    let cache = &mut sess.cache;
    let lookup = cache_op.get(&cache[pos]);
    match lookup {
        Some(res) => res,
        None => {
            eprintln!("parse[{pos}]: nothing in cache");
            let res = parser(sess, pos);
            cache_op.set(&mut sess.cache[pos], res.clone());
            res
        }
    }
}

pub fn curr_tok<Tok: Token, Derivation>(sess: &Session<Tok, Derivation>, pos: usize) -> Tok {
    match sess.source.get(pos) {
        None => Tok::EOF,
        Some(tok) => tok.clone(),
    }
}

pub fn eat<Tok: Token, Derivation>(sess: &Session<Tok, Derivation>, pos: usize) -> (usize, Tok) {
    (pos + 1, curr_tok(sess, pos))
}
