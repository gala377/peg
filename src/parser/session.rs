use crate::parser::token::Token;

#[derive(Clone)]
pub struct Session<'input, T: Token, D> {
    pub source: &'input Vec<T>,
    pub cache: ParserCache<D>,
}

// Each index corresponds to the position in the input
// So `parser_cache[0].adddition` is a result of calling addition parsing function
// when no tokens have been eaten yet.
pub(super) type ParserCache<D> = Vec<D>;
