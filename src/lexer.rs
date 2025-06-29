use std::cell::RefCell;
use std::rc::Rc;

use bstr::BStr;
use token::BinOpKind;
pub use token::{Token, TokenKind};

use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};

pub mod interner;
pub mod token;

const EOF_CHAR: u8 = b'\0';

#[derive(Debug)]
pub struct Lexer<'s> {
    src: &'s BStr,
    pos: usize,
    diag: Rc<RefCell<Diagnostics>>,
}

impl<'s> Lexer<'s> {
    pub fn new(src: &'s [u8], diag: Rc<RefCell<Diagnostics>>) -> Self {
        Self {
            src: BStr::new(src),
            pos: 0,
            diag,
        }
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
                c if c.is_ascii_alphabetic() || c == b'_' => self.read_name_or_kw(c, start),
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
                    self.error(
                        DiagErrorKind::unexpected("symbol", "valid B symbol", escape(c)),
                        Span::new(start, self.pos),
                    );
                    continue;
                }
            };

            break (kind, start);
        };

        Token::new(kind, Span::new(start, self.pos))
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

    fn read_name_or_kw(&mut self, first: u8, start: usize) -> TokenKind {
        debug_assert!(first.is_ascii_alphabetic());

        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            self.next();
        }

        TokenKind::ident(&self.src[start..self.pos])
    }

    fn read_number(&mut self, first: u8, start: usize) -> TokenKind {
        debug_assert!(first.is_ascii_digit());

        while self.peek().is_ascii_digit() {
            self.next();
        }

        TokenKind::number(&self.src[start..self.pos])
    }

    fn read_char(&mut self, start: usize) -> TokenKind {
        while !self.is_eof() && self.peek() != b'\'' {
            self.next();
        }

        let kind = TokenKind::char(&self.src[start + 1..self.pos]);

        // skip "
        self.next();
        kind
    }

    fn read_string(&mut self, start: usize) -> TokenKind {
        while !self.is_eof() && self.peek() != b'"' {
            self.next();
        }

        let kind = TokenKind::string(&self.src[start + 1..self.pos]);

        // skip "
        self.next();
        kind
    }

    fn error(&self, kind: DiagErrorKind, span: Span) {
        self.diag.borrow_mut().error(kind, span)
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
