use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    AutoDecl, Const, ConstKind, DefAst, DefKind, ExprAst, ImmVal, Name, Node, StmtAst,
};
use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};
use crate::lexer::token::Kw;
use crate::lexer::{Lexer, Token, TokenKind};

#[derive(Debug)]
pub struct Parser<'s> {
    lexer: Lexer<'s>,
    diag: Rc<RefCell<Diagnostics>>,
    next_tok: [Token; 2],
}

impl<'s> Parser<'s> {
    pub fn new(lexer: Lexer<'s>, diag: Rc<RefCell<Diagnostics>>) -> Self {
        Self {
            lexer,
            diag,
            next_tok: [Token::eof(); 2],
        }
    }

    pub fn parse_program(&mut self) -> Vec<DefAst> {
        let mut defs = Vec::new();

        self.init();
        loop {
            if let Some(name) = self.parse_name("definition") {
                let def = if self.peek_token().kind == TokenKind::OParen {
                    self.parse_def_function(name)
                } else {
                    self.parse_def_vector(name)
                };
                if let Some(def) = def {
                    defs.push(def);
                }
            } else {
                // TODO: Skip until next name
                self.next_token();
            }
        }
    }

    pub fn parse_def_function(&mut self, name: Name) -> Option<DefAst> {
        // eat '('
        self.next_token();
        let params =
            self.parse_any_comma(|p| p.parse_name("function parameter"), TokenKind::CParen)?;

        let body = self.parse_stmt()?;
        Some(DefAst {
            name,
            kind: DefKind::Function { params, body },
        })
    }

    pub fn parse_def_vector(&mut self, name: Name) -> Option<DefAst> {
        let size = if self.expect_opt(TokenKind::OBrack) {
            let size = self.parse_const_opt();
            self.expect(TokenKind::CBrack)?;
            size
        } else {
            None
        };

        let list = self.parse_any_comma(|p| p.parse_imm_val(), TokenKind::Semi)?;

        Some(DefAst {
            name,
            kind: DefKind::Vector { size, list },
        })
    }

    pub fn parse_stmt(&mut self) -> Option<Node<StmtAst>> {
        let Token { kind, span } = self.peek_token();

        let stmt = match kind {
            TokenKind::Keyword(Kw::Auto) => self.parse_stmt_auto()?,
            TokenKind::Keyword(Kw::Extrn) => self.parse_stmt_extrn()?,
            TokenKind::Keyword(Kw::Case) => self.parse_stmt_case()?,
            TokenKind::Keyword(Kw::If) => self.parse_stmt_cond()?,
            TokenKind::Keyword(Kw::While) => self.parse_stmt_while()?,
            TokenKind::Keyword(Kw::Switch) => self.parse_stmt_switch()?,
            TokenKind::Keyword(Kw::Goto) => self.parse_stmt_goto()?,
            TokenKind::Keyword(Kw::Return) => self.parse_stmt_return()?,
            TokenKind::Name(name) => {
                if self.peek2_token().kind == TokenKind::Colon {
                    self.parse_stmt_label(Name::new(name, span))?
                } else {
                    self.parse_stmt_semi()?
                }
            }
            TokenKind::OBrace => self.parse_stmt_block()?,
            _ => self.parse_stmt_semi()?,
        };

        Some(Node::stmt(stmt))
    }

    pub fn parse_stmt_auto(&mut self) -> Option<StmtAst> {
        // eat 'auto'
        self.next_token();
        let list = self.parse_one_or_more_comma(|p| p.parse_auto_decl(), TokenKind::Semi)?;

        Some(StmtAst::Auto(list))
    }

    pub fn parse_stmt_extrn(&mut self) -> Option<StmtAst> {
        // eat 'extrn'
        self.next_token();

        let list =
            self.parse_one_or_more_comma(|p| p.parse_name("external definition"), TokenKind::Semi)?;

        Some(StmtAst::Extrn(list))
    }

    pub fn parse_stmt_case(&mut self) -> Option<StmtAst> {
        // eat 'case'
        self.next_token();

        let cnst = self.parse_const()?;
        self.expect(TokenKind::Colon)?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::Case { cnst, stmt })
    }

    pub fn parse_stmt_cond(&mut self) -> Option<StmtAst> {
        // eat 'if'
        self.next_token();

        self.expect(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::CParen)?;

        let then_stmt = self.parse_stmt()?;
        let else_stmt = if self.expect_opt(TokenKind::Keyword(Kw::Else)) {
            Some(self.parse_stmt()?)
        } else {
            None
        };

        Some(StmtAst::Cond {
            cond,
            then_stmt,
            else_stmt,
        })
    }

    pub fn parse_stmt_while(&mut self) -> Option<StmtAst> {
        // eat 'while'
        self.next_token();

        self.expect(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.expect(TokenKind::CParen)?;
        let stmt = self.parse_stmt()?;

        Some(StmtAst::While { cond, stmt })
    }

    pub fn parse_stmt_switch(&mut self) -> Option<StmtAst> {
        // eat 'switch'
        self.next_token();

        let cond = self.parse_expr()?;
        let stmt = self.parse_stmt()?;

        Some(StmtAst::Switch { cond, stmt })
    }

    pub fn parse_stmt_goto(&mut self) -> Option<StmtAst> {
        // eat 'goto'
        self.next_token();

        let expr = self.parse_expr()?;
        self.expect(TokenKind::Semi)?;
        Some(StmtAst::Goto(expr))
    }

    pub fn parse_stmt_return(&mut self) -> Option<StmtAst> {
        // eat 'return'
        self.next_token();

        let expr = if self.expect_opt(TokenKind::OParen) {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::CParen)?;
            self.expect(TokenKind::Semi)?;
            Some(expr)
        } else {
            None
        };
        Some(StmtAst::Return(expr))
    }

    pub fn parse_stmt_label(&mut self, name: Name) -> Option<StmtAst> {
        // eat name
        self.next_token();

        self.expect(TokenKind::Colon)?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::Label { name, stmt })
    }

    pub fn parse_stmt_block(&mut self) -> Option<StmtAst> {
        // eat '{'
        self.next_token();

        let mut stmts = Vec::new();

        loop {
            if self.peek_token().kind == TokenKind::CBrace {
                break;
            }

            if let Some(stmt) = self.parse_stmt() {
                stmts.push(stmt);
            } else {
                // TODO: Skip until something
                self.next_token();
                return None;
            }
        }
        // eat '}'
        self.next_token();

        Some(StmtAst::Block(stmts))
    }

    pub fn parse_stmt_semi(&mut self) -> Option<StmtAst> {
        let expr = if self.expect_opt(TokenKind::Semi) {
            None
        } else {
            let expr = self.parse_expr()?;
            self.expect(TokenKind::Semi)?;
            Some(expr)
        };
        Some(StmtAst::Semi(expr))
    }

    pub fn parse_expr(&mut self) -> Option<Node<ExprAst>> {
        todo!()
    }

    pub fn parse_expr_group(&mut self) -> Option<ExprAst> {
        todo!()
    }

    pub fn parse_expr_primary(&mut self) -> Option<ExprAst> {
        let Token { kind, span } = self.peek_token();
        let expr = match kind {
            TokenKind::Name(name) => self.parse_expr_name(Name::new(name, span))?,
            TokenKind::Number(num) => {
                self.next_token();
                ExprAst::Const(Const::new(ConstKind::Number(num), span))
            }
            TokenKind::Char(char) => {
                self.next_token();
                ExprAst::Const(Const::new(ConstKind::Char(char), span))
            }
            TokenKind::String(str) => {
                self.next_token();
                ExprAst::Const(Const::new(ConstKind::String(str), span))
            }
            TokenKind::OParen => self.parse_expr_group()?,
            _ => {
                self.error(
                    DiagErrorKind::unexpected("token", "primary expression", format!("{kind}")),
                    span,
                );
                return None;
            }
        };
        Some(expr)
    }

    fn parse_expr_name(&mut self, name: Name) -> Option<ExprAst> {
        // eat name
        self.next_token();

        let expr = match self.peek_token().kind {
            TokenKind::OParen => self.parse_expr_call(name)?,
            TokenKind::OBrack => self.parse_expr_offset(name)?,
            _ => ExprAst::Name(name),
        };

        Some(expr)
    }

    fn parse_expr_call(&mut self, name: Name) -> Option<ExprAst> {
        todo!()
    }

    fn parse_expr_offset(&mut self, name: Name) -> Option<ExprAst> {
        todo!()
    }

    fn parse_one_or_more_comma<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser) -> Option<T>,
        terminator: TokenKind,
    ) -> Option<Vec<T>> {
        let mut list = Vec::new();
        list.push(parse_arg(self)?);

        loop {
            match self.peek_token().kind {
                TokenKind::Comma => {
                    self.next_token();
                }
                kind if kind == terminator => break,
                kind => {
                    let span = self.peek_token().span;
                    self.error(
                        DiagErrorKind::unexpected(
                            "separator",
                            format!(", or {}", terminator),
                            format!("{}", kind),
                        ),
                        span,
                    );
                    return None;
                }
            }

            if let Some(arg) = parse_arg(self) {
                list.push(arg);
            } else {
                // TODO: Skip until something
                self.next_token();
                return None;
            }
        }
        // eat `terminator`
        self.next_token();

        Some(list)
    }

    fn parse_any_comma<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser) -> Option<T>,
        terminator: TokenKind,
    ) -> Option<Vec<T>> {
        let mut list = Vec::new();

        if self.peek_token().kind != terminator {
            loop {
                if let Some(arg) = parse_arg(self) {
                    list.push(arg);
                } else {
                    // TODO: Skip until something
                    self.next_token();
                    return None;
                }

                match self.peek_token().kind {
                    TokenKind::Comma => {
                        self.next_token();
                    }
                    kind if kind == terminator => break,
                    kind => {
                        let span = self.peek_token().span;
                        self.error(
                            DiagErrorKind::unexpected(
                                "separator",
                                format!(", or {}", terminator),
                                format!("{}", kind),
                            ),
                            span,
                        );
                        return None;
                    }
                }
            }
        }
        // eat `terminator`
        self.next_token();

        Some(list)
    }

    fn parse_auto_decl(&mut self) -> Option<AutoDecl> {
        let name = self.parse_name("auto decl")?;
        let value = self.parse_const_opt();
        Some(AutoDecl { name, value })
    }

    fn parse_imm_val(&mut self) -> Option<ImmVal> {
        let Token { kind, span } = self.peek_token();
        Some(match kind {
            TokenKind::Name(name) => ImmVal::Name(Name::new(name, span)),
            TokenKind::Number(num) => ImmVal::Const(Const::new(ConstKind::Number(num), span)),
            TokenKind::Char(char) => ImmVal::Const(Const::new(ConstKind::Char(char), span)),
            TokenKind::String(str) => ImmVal::Const(Const::new(ConstKind::String(str), span)),
            kind => {
                self.error(
                    DiagErrorKind::unexpected("token", "name or const", format!("{kind}")),
                    span,
                );
                return None;
            }
        })
    }

    fn parse_const(&mut self) -> Option<Const> {
        let res = self.parse_const_opt();
        if res.is_none() {
            let Token { kind, span } = self.peek_token();
            self.error(
                DiagErrorKind::unexpected("token", "const", format!("{kind}")),
                span,
            );
        }
        res
    }

    fn parse_const_opt(&mut self) -> Option<Const> {
        let Token { kind, span } = self.peek_token();
        let cnst_kind = match kind {
            TokenKind::Number(num) => ConstKind::Number(num),
            TokenKind::Char(char) => ConstKind::Char(char),
            TokenKind::String(str) => ConstKind::String(str),
            _ => return None,
        };
        Some(Const {
            kind: cnst_kind,
            span,
        })
    }

    fn expect_opt(&mut self, expected: TokenKind) -> bool {
        let kind = self.peek_token().kind;
        if kind == expected {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, expected: TokenKind) -> Option<()> {
        let Token { kind, span } = self.peek_token();
        if kind == expected {
            self.next_token();
            Some(())
        } else {
            self.error(
                DiagErrorKind::unexpected("token", format!("{expected}"), format!("{kind}")),
                span,
            );
            None
        }
    }

    fn parse_name(&mut self, ty: &'static str) -> Option<Name> {
        let token = self.peek_token();
        if let TokenKind::Name(name) = token.kind {
            self.next_token();
            Some(Name::new(name, token.span))
        } else {
            self.error(
                DiagErrorKind::unexpected(ty, "name", format!("{}", token.kind)),
                token.span,
            );
            None
        }
    }

    #[inline]
    fn error(&mut self, kind: DiagErrorKind, span: Span) {
        self.diag.borrow_mut().error(kind, span)
    }

    #[inline]
    fn peek2_token(&mut self) -> Token {
        self.next_tok[1]
    }

    #[inline]
    fn peek_token(&mut self) -> Token {
        self.next_tok[0]
    }

    #[inline]
    fn next_token(&mut self) -> Token {
        let token = self.next_tok[0];
        self.next_tok[0] = self.next_tok[1];
        self.next_tok[1] = self.lexer.next_token();
        token
    }

    fn init(&mut self) {
        self.next_token();
        self.next_token();
    }
}
