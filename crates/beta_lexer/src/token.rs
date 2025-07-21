use crate::ascii::AStr;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum TokenKind<'a> {
    Ident(&'a AStr),
    Lit(Lit<'a>),
    OParen,
    CParen,
    And,
    Star,
    Bang,
    Minus,
    Plus,
    Slash,
    Percent,
    Or,
    Caret,
    LtLt,
    GtGt,
    OrOr,
    AndAnd,
    EqEq,
    BangEq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AndEq,
    OrEq,
    CaretEq,
    LtLtEq,
    GtGtEq,
    Comma,
    Semi,
    OBrack,
    CBrack,
    Dot,
    Colon,
    OBrace,
    CBrace,
    DotDotDot,
    Invalid(&'a AStr),
    Eof,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Lit<'a> {
    pub kind: LitKind,
    pub symbol: &'a AStr,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum LitKind {
    Int(IntLit),
    Char,
    Str,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct IntLit {
    pub radix: Radix,
    pub suffix_start: Option<u32>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(u8)]
pub enum Radix {
    Bin = 2,
    Oct = 8,
    Dec = 10,
    Hex = 16,
}
