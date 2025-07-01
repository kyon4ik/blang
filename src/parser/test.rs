use std::path::Path;

use bstr::BStr;

use crate::ast::print::PrettyPrinter;
use crate::ast::visit::ExprVisitor;
use crate::diagnostics::{DiagConfig, SourceMap};

use super::*;

fn parser(src: &[u8]) -> Parser {
    let src_map = SourceMap::new(src, Path::new(file!()));
    let config = DiagConfig::default();
    let diag = Rc::new(Diagnostics::new(config, src_map));
    Parser::new(src, diag)
}

fn tokens(src: &[u8]) -> impl Iterator<Item = Token> {
    let src_map = SourceMap::new(src, Path::new(file!()));
    let config = DiagConfig::default();
    let diag = Rc::new(Diagnostics::new(config, src_map));
    let mut lexer = Lexer::new(src, diag);

    std::iter::from_fn(move || {
        let token = lexer.next_token();
        if token.kind == TokenKind::Eof {
            None
        } else {
            Some(token)
        }
    })
}

fn test_expr(src: &[u8], dst: &[u8]) {
    let mut parser = parser(src);
    let mut pp = PrettyPrinter::new();
    assert!(
        parser.parse_expr().map(|e| pp.visit_expr(&e)).is_some(),
        "{} -> {:?}",
        BStr::new(src),
        tokens(src).map(|t| t.kind).collect::<Vec<_>>()
    );
    assert_eq!(pp.display(), BStr::new(dst));
}

#[test]
fn postfix() {
    test_expr(b"--a++;", b"(--() (()++ a))");
    test_expr(b"!b[10](a);", b"(! ($call ([] b 10) a))");
}

#[test]
fn assign() {
    test_expr(b"a = b = c;", b"(= a (= b c))");
    test_expr(b"a === (b =| 1) =>> c;", b"(=== a (=>> (=| b 1) c))");
}

#[test]
fn prefix() {
    test_expr(b"!-1 + 2;", b"(+ (! (- 1)) 2)");
    test_expr(b"++&(*--t);", b"(++() (& (* (--() t))))");
}

#[test]
fn infix() {
    test_expr(b"1 + 2 * 3;", b"(+ 1 (* 2 3))");
    test_expr(b"a + b * c / d - e;", b"(- (+ a (/ (* b c) d)) e)");
    test_expr(
        b"1 << 0 >= 2 < c >> 3 % 2;",
        b"(< (>= (<< 1 0) 2) (>> c (% 3 2)))",
    );
    test_expr(
        b"1*a != 2 * b | 3 & 4;",
        b"(| (!= (* 1 a) (* 2 b)) (& 3 4))",
    );
}

#[test]
fn group() {
    test_expr(b"('a' + b) * c;", b"(* (+ 'a' b) c)");
    test_expr(b"(((0)))(((0)));", b"($call 0 0)")
}
