use std::cell::RefCell;
use std::rc::Rc;

use bstr::BStr;
pub use token::{BinOp, Token, TokenKind};
use token::{MAX_NAME_LEN, MAX_NUMBER_LEN};

use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};

pub(crate) mod interner;
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
                b'\'' => self.read_char(),
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
            (b'=', b'=') => (BinOp::Eq, 2),
            (b'!', b'=') => (BinOp::Neq, 2),
            (b'<', b'=') => (BinOp::LtEq, 2),
            (b'>', b'=') => (BinOp::GtEq, 2),
            (b'<', b'<') => (BinOp::Shl, 2),
            (b'>', b'>') => (BinOp::Shr, 2),
            (b'+', _) => (BinOp::Add, 1),
            (b'-', _) => (BinOp::Sub, 1),
            (b'*', _) => (BinOp::Mul, 1),
            (b'/', _) => (BinOp::Div, 1),
            (b'%', _) => (BinOp::Rem, 1),
            (b'<', _) => (BinOp::Lt, 1),
            (b'>', _) => (BinOp::Gt, 1),
            (b'&', _) => (BinOp::And, 1),
            (b'|', _) => (BinOp::Or, 1),
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
        let mut symbols = [EOF_CHAR; MAX_NAME_LEN];
        symbols[0] = first;

        let mut i = 1;
        let mut too_long = false;
        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            if i < MAX_NAME_LEN {
                symbols[i] = self.next();
                i += 1;
            } else {
                too_long = true;
                self.next();
            }
        }

        if too_long {
            self.error(
                DiagErrorKind::other(format!("Name is longer than {} chars", MAX_NAME_LEN)),
                Span::new(start, self.pos),
            );
        }

        TokenKind::name_or_keyword(symbols)
    }

    fn read_number(&mut self, first: u8, start: usize) -> TokenKind {
        let mut digits = [EOF_CHAR; MAX_NUMBER_LEN];
        digits[0] = first;

        let mut i = 1;
        let mut too_long = false;
        while self.peek().is_ascii_digit() {
            if i < MAX_NUMBER_LEN {
                digits[i] = self.next();
                i += 1;
            } else {
                too_long = true;
                self.next();
            }
        }
        if too_long {
            self.error(
                DiagErrorKind::other(format!("Number is longer than {} digits", MAX_NUMBER_LEN)),
                Span::new(start, self.pos),
            );
        }

        TokenKind::Number(digits)
    }

    fn read_char(&mut self) -> TokenKind {
        let mut char = [EOF_CHAR; 2];
        char[0] = self.next();
        if char[0] == b'\'' {
            char[0] = EOF_CHAR;
            self.error(
                DiagErrorKind::unexpected("symbol", "character", "'"),
                Span::new(self.pos - 1, self.pos),
            );
        } else {
            char[1] = self.next();
            if char[1] != b'\'' {
                if self.peek() != b'\'' {
                    self.error(
                        DiagErrorKind::unexpected("symbol", "'", escape(self.peek())),
                        Span::new(self.pos, self.pos + 1),
                    );
                } else {
                    self.next();
                }
            } else {
                char[1] = EOF_CHAR;
            }
        }

        TokenKind::Char(char)
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
