use std::rc::Rc;

use token::{BinOpKind, Literal, LiteralKind};
pub use token::{Token, TokenKind};

use crate::diagnostics::{Diagnostics, Span};

pub mod interner;
pub mod token;

const EOF_CHAR: u8 = b'\0';

pub struct Lexer<'s> {
    src: &'s [u8],
    pos: usize,
    diag: Rc<Diagnostics>,
}

fn is_ident_start(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'_')
}

fn is_ident_continue(c: u8) -> bool {
    matches!(c, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_')
}

impl<'s> Lexer<'s> {
    pub fn new(src: &'s [u8], diag: Rc<Diagnostics>) -> Self {
        Self { src, pos: 0, diag }
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        let (kind, start) = loop {
            self.skip();
            if self.is_eof() {
                return Token::eof();
            }

            let start = self.pos;
            let kind = match self.next() {
                c if is_ident_start(c) => self.read_ident(c, start),
                c if c.is_ascii_digit() => self.read_number(c, start),
                b'\'' => self.read_char(start),
                b'\"' => self.read_string(start),
                b'=' => self.read_assign_or_eq(),
                b'(' => OParen,
                b')' => CParen,

                b'[' => OBrack,
                b']' => CBrack,

                b'{' => OBrace,
                b'}' => CBrace,

                b';' => Semi,
                b',' => Comma,
                b':' => Colon,
                b'?' => QMark,
                b'!' => self.next_if(b'=', BangEq, Bang),
                b'|' => Pipe,
                b'&' => Amps,
                b'<' => match self.peek() {
                    b'=' => {
                        self.next();
                        LtEq
                    }
                    b'<' => {
                        self.next();
                        LtLt
                    }
                    _ => Lt,
                },
                b'>' => match self.peek() {
                    b'=' => {
                        self.next();
                        GtEq
                    }
                    b'>' => {
                        self.next();
                        GtGt
                    }
                    _ => Gt,
                },
                b'-' => self.next_if(b'-', MinusMinus, Minus),
                b'+' => self.next_if(b'+', PlusPlus, Plus),
                b'%' => Percent,
                b'*' => Star,
                b'/' => Slash,

                c => {
                    self.diag
                        .error(
                            Span::from((start, self.pos)),
                            format!("symbol '{}' is not from ASCII set", escape(c)),
                        )
                        .finish();
                    continue;
                }
            };

            break (kind, start);
        };

        Token::new(kind, Span::from((start, self.pos)))
    }

    fn read_assign_or_eq(&mut self) -> TokenKind {
        let (binop, count) = match (self.peek(), self.peek2()) {
            (b'=', b'=') => (BinOpKind::Eq, 2),
            (b'!', b'=') => (BinOpKind::Neq, 2),
            (b'<', b'=') => (BinOpKind::LtEq, 2),
            (b'>', b'=') => (BinOpKind::GtEq, 2),
            (b'<', b'<') => (BinOpKind::Shl, 2),
            (b'>', b'>') => (BinOpKind::Shr, 2),
            (b'+', _) => (BinOpKind::Add, 1),
            (b'-', _) => (BinOpKind::Sub, 1),
            (b'*', _) => (BinOpKind::Mul, 1),
            (b'/', _) => (BinOpKind::Div, 1),
            (b'%', _) => (BinOpKind::Rem, 1),
            (b'<', _) => (BinOpKind::Lt, 1),
            (b'>', _) => (BinOpKind::Gt, 1),
            (b'&', _) => (BinOpKind::And, 1),
            (b'|', _) => (BinOpKind::Or, 1),
            (b'=', _) => {
                self.next();
                return TokenKind::EqEq;
            }
            (_, _) => return TokenKind::Eq,
        };

        if count > 1 {
            self.next();
        }
        if count > 0 {
            self.next();
        }
        TokenKind::Assign(binop)
    }

    fn read_ident(&mut self, first: u8, start: usize) -> TokenKind {
        debug_assert!(is_ident_start(first));

        while is_ident_continue(self.peek()) {
            self.next();
        }

        TokenKind::ident(&self.src[start..self.pos])
    }

    fn read_number(&mut self, first: u8, start: usize) -> TokenKind {
        debug_assert!(first.is_ascii_digit());

        while self.peek().is_ascii_digit() {
            self.next();
        }

        TokenKind::Literal(Literal::new(
            LiteralKind::Number,
            &self.src[start..self.pos],
        ))
    }

    fn read_char(&mut self, start: usize) -> TokenKind {
        while !self.is_eof() && self.peek() != b'\'' {
            if self.peek() == b'*' {
                self.next();
            }
            self.next();
        }

        let kind = TokenKind::Literal(Literal::new(
            LiteralKind::Char,
            &self.src[start + 1..self.pos],
        ));

        // skip "
        self.next();
        kind
    }

    fn read_string(&mut self, start: usize) -> TokenKind {
        while !self.is_eof() && self.peek() != b'"' {
            if self.peek() == b'*' {
                self.next();
            }
            self.next();
        }

        let kind = TokenKind::Literal(Literal::new(
            LiteralKind::String,
            &self.src[start + 1..self.pos],
        ));

        // skip "
        self.next();
        kind
    }

    fn next_if(&mut self, expect: u8, success: TokenKind, fail: TokenKind) -> TokenKind {
        if self.peek() == expect {
            self.next();
            success
        } else {
            fail
        }
    }

    fn is_eof(&self) -> bool {
        self.pos >= self.src.len()
    }

    fn skip(&mut self) {
        loop {
            match self.peek() {
                b'/' => {
                    if self.peek2() == b'*' {
                        self.next();
                        self.next();
                        self.skip_comment();
                    } else {
                        break;
                    }
                }
                c if c.is_ascii_whitespace() => {
                    self.next();
                    self.skip_whitespaces();
                }
                _ => break,
            }
        }
    }

    fn skip_comment(&mut self) {
        while self.peek() != b'*' || self.peek2() != b'/' {
            self.next();
        }
        self.next();
        self.next();
    }

    fn skip_whitespaces(&mut self) {
        while self.peek().is_ascii_whitespace() {
            self.next();
        }
    }

    fn next(&mut self) -> u8 {
        let c = *self.src.get(self.pos).unwrap_or(&EOF_CHAR);
        self.pos += 1;
        c
    }

    fn peek(&self) -> u8 {
        *self.src.get(self.pos).unwrap_or(&EOF_CHAR)
    }

    fn peek2(&self) -> u8 {
        *self.src.get(self.pos + 1).unwrap_or(&EOF_CHAR)
    }
}

fn escape(c: u8) -> String {
    String::from_utf8_lossy(&c.escape_ascii().collect::<Vec<_>>()).into_owned()
}
