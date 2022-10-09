#[macro_use]
mod peg_macro;
mod session;
mod token;

use token::Token;
pub use session::Session;
use crate::property::Property;

#[derive(Clone, Debug)]
pub enum ParseRes<T> {
    Parsed { val: T, next_pos: usize },
    NoParse,
}


pub fn parse<R: Clone, T: Token, D>(
    sess: &mut Session<T, D>,
    pos: usize,
    cache_op: impl Property<D, Option<ParseRes<R>>, ParseRes<R>>,
    parser: impl Fn(&mut Session<T, D>, usize) -> ParseRes<R>,
) -> ParseRes<R> {
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

pub fn curr_tok<T: Token, D>(sess: &Session<T, D>, pos: usize) -> T {
    match sess.source.get(pos) {
        None => T::EOF,
        Some(tok) => tok.clone(),
    }
}

pub fn eat<T: Token, D>(sess: &Session<T, D>, pos: usize) -> (usize, T) {
    (pos + 1, curr_tok(sess, pos))
}