use token::{MAX_NAME_LEN, MAX_NUMBER_LEN};
pub use token::{Span, Token, TokenKind};

pub(crate) mod interner;
pub mod token;

const EOF_CHAR: u8 = b'\0';

pub struct Lexer<'s> {
    src: &'s [u8],
    pos: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(src: &'s [u8]) -> Self {
        Self { src, pos: 0 }
    }

    pub fn next_token(&mut self) -> Token {
        use TokenKind::*;

        self.skip();
        if self.is_eof() {
            return Token::eof();
        }

        let start = self.pos;
        let kind = match self.next() {
            c if c.is_ascii_alphabetic() || c == b'_' => self.read_name_or_kw(c),
            c if c.is_ascii_digit() => self.read_number(c),
            b'\'' => self.read_char(),
            b'\"' => self.read_string(),
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
            b'=' => self.next_if(b'=', EqEq, Eq),
            b'<' => match self.peek() {
                b'=' => LtEq,
                b'<' => LtLt,
                _ => Lt,
            },
            b'>' => match self.peek() {
                b'=' => GtEq,
                b'>' => GtGt,
                _ => Gt,
            },
            b'-' => self.next_if(b'-', MinusMinus, Minus),
            b'+' => self.next_if(b'+', PlusPlus, Plus),
            b'%' => Percent,
            b'*' => Star,
            b'/' => Slash,

            // TODO: better diagnostics
            c => panic!("Unexpected symbol: {}", escape(c)),
        };

        Token::new(kind, Span::new(start, self.pos))
    }

    fn read_name_or_kw(&mut self, first: u8) -> TokenKind {
        let mut symbols = [EOF_CHAR; MAX_NAME_LEN];
        symbols[0] = first;

        let mut i = 1;
        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            if i < MAX_NAME_LEN {
                symbols[i] = self.next();
                i += 1;
            } else {
                self.next();
                // TODO: diagnostics
            }
        }

        TokenKind::name_or_keyword(symbols)
    }

    fn read_number(&mut self, first: u8) -> TokenKind {
        let mut digits = [EOF_CHAR; MAX_NUMBER_LEN];
        digits[0] = first;

        let mut i = 1;
        while self.peek().is_ascii_digit() {
            if i < MAX_NUMBER_LEN {
                digits[i] = self.next();
                i += 1;
            } else {
                self.next();
                // TODO: diagnostics
            }
        }

        TokenKind::Number(digits)
    }

    fn read_char(&mut self) -> TokenKind {
        let mut char = [EOF_CHAR; 2];
        char[0] = self.next();
        if char[0] == b'\'' {
            char[0] = EOF_CHAR;
            // TODO: better diagnostics
            panic!("Expected character, found '");
        } else {
            char[1] = self.next();
            if char[1] != b'\'' {
                if self.peek() != b'\'' {
                    // TODO: better diagnostics
                    panic!("Expected ', found {}", escape(self.peek()));
                } else {
                    self.next();
                }
            } else {
                char[1] = EOF_CHAR;
            }
        }

        TokenKind::Char(char)
    }

    fn read_string(&mut self) -> TokenKind {
        // FIXME: Do not use Vec here, allocate once
        let mut string = Vec::new();

        while !self.is_eof() && self.peek() != b'"' {
            string.push(self.next());
        }

        self.next();

        TokenKind::string(&string)
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
    let bytes = c.escape_ascii().collect::<Vec<_>>();
    String::from_utf8(bytes).unwrap()
}
