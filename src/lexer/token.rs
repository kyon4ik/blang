use std::fmt;

use bstr::BStr;
use strum_macros::{EnumCount, EnumDiscriminants};

use crate::diagnostics::Span;

use super::interner::InternedStr;

pub const MAX_NAME_LEN: usize = 8;
pub const MAX_CHAR_LEN: usize = 2;

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumDiscriminants)]
pub enum TokenKind {
    Name(InternedStr),
    Number(InternedStr),
    Char(InternedStr),
    String(InternedStr),
    Keyword(Kw),
    Assign(BinOpKind),

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

#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumCount)]
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
pub enum BinOpKind {
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

impl BinOpKind {
    pub fn binding_power(&self) -> (u8, u8) {
        match self {
            Self::Or => (10, 11),
            Self::And => (12, 13),
            Self::Eq => (14, 15),
            Self::Neq => (14, 15),
            Self::Lt => (16, 17),
            Self::LtEq => (16, 17),
            Self::Gt => (16, 17),
            Self::GtEq => (16, 17),
            Self::Shl => (18, 19),
            Self::Shr => (18, 19),
            Self::Add => (20, 21),
            Self::Sub => (20, 21),
            Self::Rem => (22, 23),
            Self::Mul => (22, 23),
            Self::Div => (22, 23),
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

    pub fn ident(symbols: &[u8]) -> Self {
        match InternedStr::new(symbols).to_keyword() {
            Ok(kw) => Self::Keyword(kw),
            Err(id) => Self::Name(id),
        }
    }

    pub fn number(symbols: &[u8]) -> Self {
        Self::Number(InternedStr::new(symbols))
    }

    pub fn char(str: &BStr) -> Self {
        Self::Char(InternedStr::new(str))
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
            Self::Char(char) => write!(f, "Char({:x} {})", char.index(), char.display()),
            Self::String(string) => {
                write!(f, "String({:x} \"{}\")", string.index(), string.display())
            }
            other => write!(f, "{other:?}"),
        }
    }
}
