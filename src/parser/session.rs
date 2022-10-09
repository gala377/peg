use crate::parser::token::Token;

#[derive(Clone)]
pub struct Session<'input, TokenTyp: Token, Derivation> {
    pub source: &'input Vec<TokenTyp>,
    pub cache: ParserCache<Derivation>,
}

// Each index corresponds to the position in the input
// So `parser_cache[0].adddition` is a result of calling addition parsing function
// when no tokens have been eaten yet.
pub(super) type ParserCache<T> = Vec<T>;
