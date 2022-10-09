#[macro_export]
macro_rules! field_op {
    ($tname:ident.$fname:ident) => {
        (
            |d: &$tname| d.$fname.clone(),
            |d: &mut $tname, v: ParseRes<_>| d.$fname = Some(v),
        )
    };
}
#[macro_export]
macro_rules! parse {
    ($sess:expr, $pos:expr, $fname:ident) => {
        parse($sess, $pos, $crate::field_op!(Self.$fname), Self::$fname)
    };

    ($sess:expr, $pos:expr, $tname:ident::$fname:ident) => {
        parse(
            $sess,
            $pos,
            $crate::field_op!($tname.$fname),
            $tname::$fname,
        )
    };
}

#[macro_export]
macro_rules! or {
    ($sess:expr, $pos:expr, $($rule:ident),+) => {
        $crate::or!(@unpack $sess, $pos, $($rule),+)
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident, $($rest:ident),+) => {
        match $crate::parse!($sess, $pos, $rule) {
            res@ParseRes::Parsed { .. } => res,
            ParseRes::NoParse => {
                $crate::or!(@unpack $sess, $pos, $($rest),+)
            }
        }
    };
    (@unpack $sess:expr, $pos:expr, $rule:ident) => {
        $crate::parse!($sess, $pos, $rule)
    };
}

macro_rules! peg {
    (grammar $name:ident { $($rule:ident($sess:pat, $pos:pat) -> $t:ty $body:block)+ }) => {

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
                        $crate::field_op!($name.$rule),
                        f)
                }
            )+
        }
    };
}
