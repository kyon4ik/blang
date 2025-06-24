use std::fmt;
use std::sync::LazyLock;

use bstr::BStr;

use super::interner::{InternedStr, StringInterner};

pub const MAX_NAME_LEN: usize = 8;
pub const MAX_CHAR_LEN: usize = 2;

// Not defined in reference, assume max word has 64 bits
pub const MAX_NUMBER_LEN: usize = u64::MAX.ilog10() as usize;

const INTERNER_ARENA_SIZE: usize = 1024 * 1024; // 1MB

static INTERNER: LazyLock<StringInterner> =
    LazyLock::new(|| StringInterner::new(INTERNER_ARENA_SIZE));

// TODO: use trie
const KEYWORDS: [([u8; MAX_NAME_LEN], Kw); 8] = [
    (*b"auto\0\0\0\0", Kw::Auto),
    (*b"extrn\0\0\0", Kw::Extrn),
    (*b"case\0\0\0\0", Kw::Case),
    (*b"if\0\0\0\0\0\0", Kw::If),
    (*b"else\0\0\0\0", Kw::Else),
    (*b"while\0\0\0", Kw::While),
    (*b"switch\0\0", Kw::Switch),
    (*b"goto\0\0\0\0", Kw::Goto),
];

#[derive(Clone, Copy, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TokenKind {
    Name([u8; MAX_NAME_LEN]),
    Number([u8; MAX_NUMBER_LEN]),
    Char([u8; MAX_CHAR_LEN]),
    String(InternedStr),
    Keyword(Kw),

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
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self::new(0, 0)
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
        Self::String(INTERNER.intern(str))
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Name(name) => write!(f, "Name({})", BStr::new(name)),
            Self::Number(number) => write!(f, "Number({})", BStr::new(number)),
            Self::Char(char) => write!(f, "Char({})", BStr::new(char)),
            Self::String(string) => write!(
                f,
                "String({:x} \"{}\")",
                string.index(),
                BStr::new(INTERNER.get_string(*string))
            ),
            other => write!(f, "{:?}", other),
        }
    }
}
