use std::fmt;

use bstr::BStr;
use strum_macros::EnumDiscriminants;

use crate::diagnostics::Span;

use super::interner::InternedStr;

pub const MAX_NAME_LEN: usize = 8;
pub const MAX_CHAR_LEN: usize = 2;

// TODO: use trie
const KEYWORDS: [(&[u8], Kw); 9] = [
    (b"auto", Kw::Auto),
    (b"extrn", Kw::Extrn),
    (b"case", Kw::Case),
    (b"if", Kw::If),
    (b"else", Kw::Else),
    (b"while", Kw::While),
    (b"switch", Kw::Switch),
    (b"goto", Kw::Goto),
    (b"return", Kw::Return),
];

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumDiscriminants)]
pub enum TokenKind {
    Name(InternedStr),
    Number(InternedStr),
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
    pub fn dummy_name() -> Self {
        Self::Name(InternedStr::dummy())
    }

    pub fn name_or_keyword(symbols: &[u8]) -> Self {
        if let Some(kw) = KEYWORDS
            .iter()
            .find_map(|(word, kw)| word.eq(&symbols).then_some(kw))
        {
            Self::Keyword(*kw)
        } else {
            Self::Name(InternedStr::new(symbols))
        }
    }

    pub fn number(symbols: &[u8]) -> Self {
        Self::Number(InternedStr::new(symbols))
    }

    pub fn string(str: &BStr) -> Self {
        Self::String(InternedStr::new(str))
    }

    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Keyword(a), Self::Keyword(b)) => a.eq(b),
            (Self::Assign(a), Self::Assign(b)) => a.eq(b),
            _ => TokenKindDiscriminants::from(self).eq(&TokenKindDiscriminants::from(other)),
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "Name({:x} {})", name.index(), name.display()),
            Self::Number(number) => write!(f, "Number({:x} {})", number.index(), number.display()),
            Self::Char(char) => write!(f, "Char({})", BStr::new(char)),
            Self::String(string) => {
                write!(f, "String({:x} \"{}\")", string.index(), string.display())
            }
            other => write!(f, "{other:?}"),
        }
    }
}
