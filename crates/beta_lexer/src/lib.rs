use ascii::AChar;

pub mod ascii;
pub mod keyword;
pub mod symbol;
pub mod token;

#[inline]
pub const fn is_ident_continue(c: AChar) -> bool {
    matches!(c as u8, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

#[inline]
pub const fn is_ident_start(c: AChar) -> bool {
    matches!(c as u8, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

#[inline]
pub const fn is_alpha(c: AChar) -> bool {
    (c as u8).is_ascii_alphabetic()
}

#[inline]
pub const fn is_hex_digit(c: AChar) -> bool {
    (c as u8).is_ascii_hexdigit()
}

#[inline]
pub const fn is_digit(c: AChar) -> bool {
    (c as u8).is_ascii_digit()
}

#[inline]
pub const fn is_whitespace(c: AChar) -> bool {
    // `is_ascii_whitespace` does not cover `LineTabulation` case
    use AChar::*;
    matches!(
        c,
        CharacterTabulation | LineFeed | LineTabulation | FormFeed | CarriageReturn | Space
    )
}
