use std::fmt;

use bstr::BStr;

use crate::diagnostics::Span;

use super::interner::InternedStr;

pub const MAX_NAME_LEN: usize = 8;
pub const MAX_CHAR_LEN: usize = 2;

// Not defined in reference, assume max word has 64 bits
// FIXME: octal form may have more digits
pub const MAX_NUMBER_LEN: usize = u64::MAX.ilog10() as usize;

// TODO: use trie
const KEYWORDS: [([u8; MAX_NAME_LEN], Kw); 9] = [
    (*b"auto\0\0\0\0", Kw::Auto),
    (*b"extrn\0\0\0", Kw::Extrn),
    (*b"case\0\0\0\0", Kw::Case),
    (*b"if\0\0\0\0\0\0", Kw::If),
    (*b"else\0\0\0\0", Kw::Else),
    (*b"while\0\0\0", Kw::While),
    (*b"switch\0\0", Kw::Switch),
    (*b"goto\0\0\0\0", Kw::Goto),
    (*b"return\0\0", Kw::Return),
];

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Name([u8; MAX_NAME_LEN]),
    Number([u8; MAX_NUMBER_LEN]),
    Char([u8; MAX_CHAR_LEN]),
    String(InternedStr),
    Keyword(Kw),
    Assign(BinOp),

    // ( ... )
    OParen,
    CParen,

    // [ ... ]
    OBrack,
    CBrack,

    // { ... }
    OBrace,
    CBrace,

    // ;
    Semi,
    // ,
    Comma,
    // :
    Colon,
    // ?
    QMark,
    // !
    Bang,
    // ++
    PlusPlus,
    // --
    MinusMinus,
    // |
    Pipe,
    // &
    Amps,
    // =
    Eq,
    // ==
    EqEq,
    // !=
    BangEq,
    // <
    Lt,
    // <=
    LtEq,
    // >
    Gt,
    // >=
    GtEq,
    // <<
    LtLt,
    // >>
    GtGt,
    // -
    Minus,
    // +
    Plus,
    // %
    Percent,
    // *
    Star,
    // /
    Slash,

    Eof,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Kw {
    Auto,
    Extrn,
    Case,
    If,
    Else,
    While,
    Switch,
    Goto,
    Return,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BinOp {
    Or,   // |
    And,  // &
    Eq,   // ==
    Neq,  // !=
    Lt,   // <
    LtEq, // <=
    Gt,   // >
    GtEq, // >=
    Shl,  // <<
    Shr,  // >>
    Add,  // +
    Sub,  // -
    Rem,  // %
    Mul,  // *
    Div,  // /
}

impl BinOp {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            BinOp::Or => (10, 11),
            BinOp::And => (12, 13),
            BinOp::Eq => (14, 15),
            BinOp::Neq => (14, 15),
            BinOp::Lt => (16, 17),
            BinOp::LtEq => (16, 17),
            BinOp::Gt => (16, 17),
            BinOp::GtEq => (16, 17),
            BinOp::Shl => (18, 19),
            BinOp::Shr => (18, 19),
            BinOp::Add => (20, 21),
            BinOp::Sub => (20, 21),
            BinOp::Rem => (22, 23),
            BinOp::Mul => (22, 23),
            BinOp::Div => (22, 23),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn eof() -> Self {
        Self::new(TokenKind::Eof, Span::empty())
    }
}

impl TokenKind {
    pub fn name_or_keyword(symbols: [u8; MAX_NAME_LEN]) -> Self {
        if let Some(kw) = KEYWORDS
            .iter()
            .find_map(|(word, kw)| word.eq(&symbols).then_some(kw))
        {
            Self::Keyword(*kw)
        } else {
            Self::Name(symbols)
        }
    }

    pub fn string(str: &BStr) -> Self {
        Self::String(InternedStr::new(str))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "Name({})", BStr::new(name)),
            Self::Number(number) => write!(f, "Number({})", BStr::new(number)),
            Self::Char(char) => write!(f, "Char({})", BStr::new(char)),
            Self::String(string) => {
                write!(f, "String({:x} \"{}\")", string.index(), string.display())
            }
            other => write!(f, "{:?}", other),
        }
    }
}
