#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
#[repr(u8)]
pub enum Kw {
    Cast,
    Const,
    Default,
    Else,
    Extern,
    False,
    Fn,
    Global,
    If,
    Let,
    Static,
    Struct,
    Switch,
    True,
    While,
}
