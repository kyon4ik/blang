use std::fmt;

use strum_macros::{Display, EnumCount, EnumDiscriminants, IntoStaticStr};

use crate::diagnostics::Span;

use super::interner::InternedStr;

pub const MAX_NAME_LEN: usize = 8;
pub const MAX_CHAR_LEN: usize = 2;

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, EnumDiscriminants, Display)]
pub enum TokenKind {
    Name(InternedStr),
    Literal(Literal),
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, IntoStaticStr)]
pub enum BinOpKind {
    #[strum(serialize = "|")]
    Or, // |
    #[strum(serialize = "&")]
    And, // &
    #[strum(serialize = "==")]
    Eq, // ==
    #[strum(serialize = "!=")]
    Neq, // !=
    #[strum(serialize = "<")]
    Lt, // <
    #[strum(serialize = "<=")]
    LtEq, // <=
    #[strum(serialize = ">")]
    Gt, // >
    #[strum(serialize = ">=")]
    GtEq, // >=
    #[strum(serialize = "<<")]
    Shl, // <<
    #[strum(serialize = ">>")]
    Shr, // >>
    #[strum(serialize = "+")]
    Add, // +
    #[strum(serialize = "-")]
    Sub, // -
    #[strum(serialize = "%")]
    Rem, // %
    #[strum(serialize = "*")]
    Mul, // *
    #[strum(serialize = "/")]
    Div, // /
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

    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Keyword(a), Self::Keyword(b)) => a.eq(b),
            (Self::Assign(a), Self::Assign(b)) => a.eq(b),
            _ => TokenKindDiscriminants::from(self).eq(&TokenKindDiscriminants::from(other)),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Display)]
pub enum LiteralKind {
    Number,
    Char,
    String,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: InternedStr,
}

impl Literal {
    pub fn new(kind: LiteralKind, value: &[u8]) -> Self {
        Self {
            kind,
            value: InternedStr::new(value),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}#{}: {}",
            self.kind,
            self.value.index(),
            String::from_utf8_lossy(self.value.display())
        )
    }
}
