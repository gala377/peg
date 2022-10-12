#[macro_export]
macro_rules! field_op {
    ($tname:ident.$fname:ident) => {
        (
            |d: &$tname| d.$fname.clone(),
            |d: &mut $tname, v: $crate::parser::ParseRes<_>| d.$fname = Some(v),
        )
    };
}
#[macro_export]
macro_rules! parse {
    ($sess:expr, $pos:expr, $fname:ident) => {
        $crate::parser::parse($sess, $pos, $crate::field_op!(Self.$fname), Self::$fname)
    };

    ($sess:expr, $pos:expr, $tname:ident::$fname:ident) => {
        $crate::parser::parse(
            $sess,
            $pos,
            $crate::field_op!($tname.$fname),
            $tname::$fname,
        )
    };
}

/// Should be used only in the `peg` macro rules' bodies as it does not
/// use parse! macro for memoization. Instead it assumes that the rule
/// called handles memoization itself.
#[macro_export]
macro_rules! or {
    ($sess:expr, $pos:expr, $($rule:ident),+) => {
        $crate::or!(@unpack $sess, $pos, $($rule),+)
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident, $($rest:ident),+) => {
        match Self::$rule($sess, $pos) {
            res@$crate::parser::ParseRes::Parsed { .. } => res,
            $crate::parser::ParseRes::NoParse => {
                $crate::or!(@unpack $sess, $pos, $($rest),+)
            }
        }
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident) => {
        Self::$rule($sess, $pos)
    };
}

/// Same as `or!` but calls rules with the `parse!` macro so the rules
/// do not need to handle memoization themselves.
#[macro_export]
macro_rules! or_parse {
    ($sess:expr, $pos:expr, $($rule:ident),+) => {
        $crate::or!(@unpack $sess, $pos, $($rule),+)
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident, $($rest:ident),+) => {
        match crate::parse!($sess, $pos, $rule) {
            res@$crate::parser::ParseRes::Parsed { .. } => res,
            $crate::parser::ParseRes::NoParse => {
                $crate::or!(@unpack $sess, $pos, $($rest),+)
            }
        }
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident) => {
        $crate::parse!($sess, $pos, $rule)
    };
}

#[macro_export]
macro_rules! peg {
    (grammar $name:ident { $($rvis:vis $rule:ident($sess:pat, $pos:pat) -> $t:ty $body:block)+ }) => {

        #[derive(Default, Clone, Debug)]
        pub struct $name {
            $(
                $rule: Option<$crate::parser::ParseRes<$t>>
            ),+
        }
        impl $name {
            $(
                $rvis fn $rule(sess: &mut $crate::parser::Session<char, $name>, pos: usize) -> $crate::parser::ParseRes<$t> {
                    let f = |$sess: &mut $crate::parser::Session<char, $name>, $pos: usize| -> $crate::parser::ParseRes<$t> { $body };
                    parse(
                        sess,
                        pos,
                        $crate::field_op!($name.$rule),
                        f)
                }
            )+
        }
    };
}
