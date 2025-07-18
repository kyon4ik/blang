use std::fmt;
use std::rc::Rc;

use crate::ast::*;
use crate::diagnostics::{Diagnostics, Span};
use crate::lexer::token::{BinOpKind, Kw};
use crate::lexer::{Lexer, Token, TokenKind};

#[cfg(test)]
mod test;

pub struct Parser<'s> {
    lexer: Lexer<'s>,
    diag: Rc<Diagnostics>,
    next_tok: [Token; 2],
}

impl<'s> Parser<'s> {
    pub fn new(src: &'s [u8], diag: Rc<Diagnostics>) -> Self {
        let mut lexer = Lexer::new(src, diag.clone());
        let next_tok = [lexer.next_token(), lexer.next_token()];
        Self {
            lexer,
            diag,
            next_tok,
        }
    }

    pub fn parse_program(&mut self) -> Vec<DefAst> {
        let mut defs = Vec::new();

        while !self.is_eof() {
            if let Some(name) = self.parse_name() {
                let def = if self.peek().kind == TokenKind::OParen {
                    self.parse_def_function(name)
                } else {
                    self.parse_def_vector(name)
                };

                if let Some(def) = def {
                    defs.push(def);
                }
            } else {
                self.synchronize_def();
            }
        }

        defs
    }

    pub fn parse_def_function(&mut self, name: Name) -> Option<DefAst> {
        let open_span = self.eat(TokenKind::OParen).span;
        let (params, close_tok) = self.parse_any_comma(|p| p.parse_name(), TokenKind::CParen)?;
        let body = self.parse_stmt()?;
        Some(DefAst::new(
            name,
            DefKind::Function {
                params: FuncParams {
                    params,
                    open_span,
                    close_span: close_tok.span,
                },
                body,
            },
        ))
    }

    pub fn parse_def_vector(&mut self, name: Name) -> Option<DefAst> {
        let size = match self.try_consume(TokenKind::OBrack) {
            Ok(_) => {
                let size = self.try_parse_const();
                let close_span = self.consume(TokenKind::CBrack)?.span;
                size.map(|sz| VecSize::Def {
                    lit: sz,
                    span: close_span,
                })
                .unwrap_or(VecSize::Empty(close_span))
            }
            Err(_) => VecSize::Undef,
        };

        let (list, _) = self.parse_any_comma(|p| p.parse_imm_val(), TokenKind::Semi)?;

        Some(DefAst::new(name, DefKind::Vector { size, list }))
    }

    pub fn parse_stmt(&mut self) -> Option<Node<StmtAst>> {
        let token = self.peek();
        let stmt = match token.kind {
            TokenKind::Keyword(Kw::Auto) => self.parse_stmt_auto(),
            TokenKind::Keyword(Kw::Extrn) => self.parse_stmt_extrn(),
            TokenKind::Keyword(Kw::Case) => self.parse_stmt_case(),
            TokenKind::Keyword(Kw::If) => self.parse_stmt_cond(),
            TokenKind::Keyword(Kw::While) => self.parse_stmt_while(),
            TokenKind::Keyword(Kw::Switch) => self.parse_stmt_switch(),
            TokenKind::Keyword(Kw::Goto) => self.parse_stmt_goto(),
            TokenKind::Keyword(Kw::Return) => self.parse_stmt_return(),
            TokenKind::Name(name) if self.peek2().kind == TokenKind::Colon => {
                self.parse_stmt_label(Name::new(name, token.span))
            }
            TokenKind::OBrace => self.parse_stmt_block(),
            _ => self.parse_stmt_semi(),
        };
        let Some(stmt) = stmt else {
            self.synchronize_stmt();
            return None;
        };

        Some(Node::stmt(stmt))
    }

    pub fn parse_stmt_auto(&mut self) -> Option<StmtAst> {
        let auto_span = self.eat(TokenKind::Keyword(Kw::Auto)).span;
        let (decls, semi) =
            self.parse_one_or_more_comma(|p| p.parse_auto_decl(), TokenKind::Semi)?;
        Some(StmtAst::new(StmtKind::Auto(AutoStmt {
            decls,
            auto_span,
            semi_span: semi.span,
        })))
    }

    pub fn parse_stmt_extrn(&mut self) -> Option<StmtAst> {
        let extrn_span = self.eat(TokenKind::Keyword(Kw::Extrn)).span;
        let (names, semi) = self.parse_one_or_more_comma(|p| p.parse_name(), TokenKind::Semi)?;
        Some(StmtAst::new(StmtKind::Extrn(ExtrnStmt {
            names,
            extrn_span,
            semi_span: semi.span,
        })))
    }

    pub fn parse_stmt_case(&mut self) -> Option<StmtAst> {
        let case_span = self.eat(TokenKind::Keyword(Kw::Case)).span;
        let cnst = self.parse_const()?;
        self.consume(TokenKind::Colon)?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::new(StmtKind::Case(CaseStmt {
            cnst,
            stmt,
            case_span,
        })))
    }

    pub fn parse_stmt_cond(&mut self) -> Option<StmtAst> {
        let if_span = self.eat(TokenKind::Keyword(Kw::If)).span;

        self.consume(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.consume(TokenKind::CParen)?;

        let then_stmt = self.parse_stmt()?;
        let else_stmt = match self.try_consume(TokenKind::Keyword(Kw::Else)) {
            Ok(_) => Some(self.parse_stmt()?),
            Err(_) => None,
        };

        Some(StmtAst::new(StmtKind::Cond(CondStmt {
            cond,
            then_stmt,
            else_stmt,
            if_span,
        })))
    }

    pub fn parse_stmt_while(&mut self) -> Option<StmtAst> {
        let while_span = self.eat(TokenKind::Keyword(Kw::While)).span;

        self.consume(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.consume(TokenKind::CParen)?;

        let stmt = self.parse_stmt()?;
        Some(StmtAst::new(StmtKind::While(WhileStmt {
            cond,
            stmt,
            while_span,
        })))
    }

    pub fn parse_stmt_switch(&mut self) -> Option<StmtAst> {
        let switch_span = self.eat(TokenKind::Keyword(Kw::Switch)).span;
        let cond = self.parse_expr()?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::new(StmtKind::Switch(SwitchStmt {
            cond,
            stmt,
            switch_span,
        })))
    }

    pub fn parse_stmt_goto(&mut self) -> Option<StmtAst> {
        let goto_span = self.eat(TokenKind::Keyword(Kw::Goto)).span;
        let label = self.parse_name()?;
        let semi_span = self.consume(TokenKind::Semi)?.span;
        Some(StmtAst::new(StmtKind::Goto(GotoStmt {
            label,
            goto_span,
            semi_span,
        })))
    }

    pub fn parse_stmt_return(&mut self) -> Option<StmtAst> {
        let return_span = self.eat(TokenKind::Keyword(Kw::Return)).span;

        let expr = match self.try_consume(TokenKind::OParen) {
            Ok(_) => {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::CParen)?;
                Some(expr)
            }
            Err(_) => None,
        };
        let semi_span = self.consume(TokenKind::Semi)?.span;
        Some(StmtAst::new(StmtKind::Return(ReturnStmt {
            expr,
            return_span,
            semi_span,
        })))
    }

    pub fn parse_stmt_label(&mut self, name: Name) -> Option<StmtAst> {
        self.eat(TokenKind::dummy_name());
        self.eat(TokenKind::Colon);
        let stmt = self.parse_stmt()?;
        Some(StmtAst::new(StmtKind::Label(LabelStmt { name, stmt })))
    }

    pub fn parse_stmt_block(&mut self) -> Option<StmtAst> {
        let open_span = self.eat(TokenKind::OBrace).span;

        let mut stmts = Vec::new();
        while self.peek().kind != TokenKind::CBrace {
            stmts.push(self.parse_stmt()?);
        }
        let close_span = self.eat(TokenKind::CBrace).span;

        Some(StmtAst::new(StmtKind::Block(BlockStmt {
            stmts,
            open_span,
            close_span,
        })))
    }

    pub fn parse_stmt_semi(&mut self) -> Option<StmtAst> {
        let (expr, semi_span) = match self.try_consume(TokenKind::Semi) {
            Ok(token) => (None, token.span),
            Err(token) => {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::Semi)?;
                (Some(expr), token.span)
            }
        };
        Some(StmtAst::new(StmtKind::Semi(SemiStmt { expr, semi_span })))
    }

    pub fn parse_expr(&mut self) -> Option<Node<ExprAst>> {
        self.parse_expr_assign()
    }

    pub fn parse_expr_assign(&mut self) -> Option<Node<ExprAst>> {
        let lhs = self.parse_expr_binary(0)?;

        // FIXME: you consume_any smh?
        let op_token = self.peek();
        let op_kind = match op_token.kind {
            TokenKind::Assign(bin_op) => Some(bin_op),
            TokenKind::Eq => None,
            kind => {
                if follows_expr(kind) {
                    return Some(lhs);
                }

                self.error_unexpected_str(op_token.span, "assignment operator", kind);
                return None;
            }
        };

        // eat `op`
        self.next();

        let op = AssignOp {
            kind: op_kind,
            span: op_token.span,
        };
        let rhs = self.parse_expr_assign()?;
        Some(Node::expr(ExprAst::new(ExprKind::Assign(AssignExpr {
            op,
            lhs,
            rhs,
        }))))
    }

    #[inline]
    fn parse_expr_binary(&mut self, min_bp: u8) -> Option<Node<ExprAst>> {
        let mut lhs = self.parse_expr_primary()?;

        loop {
            let op_token = self.peek();

            // FIXME: Hack for ternary
            if op_token.kind != TokenKind::QMark {
                let op_kind = match op_token.kind {
                    TokenKind::Plus => BinOpKind::Add,
                    TokenKind::Star => BinOpKind::Mul,
                    TokenKind::Minus => BinOpKind::Sub,
                    TokenKind::Slash => BinOpKind::Div,
                    TokenKind::Percent => BinOpKind::Rem,
                    TokenKind::Amps => BinOpKind::And,
                    TokenKind::Pipe => BinOpKind::Or,
                    TokenKind::LtLt => BinOpKind::Shl,
                    TokenKind::GtGt => BinOpKind::Shr,
                    TokenKind::EqEq => BinOpKind::Eq,
                    TokenKind::BangEq => BinOpKind::Neq,
                    TokenKind::LtEq => BinOpKind::LtEq,
                    TokenKind::GtEq => BinOpKind::GtEq,
                    TokenKind::Lt => BinOpKind::Lt,
                    TokenKind::Gt => BinOpKind::Gt,
                    // assign expr next
                    TokenKind::Assign(_) | TokenKind::Eq => break,
                    kind => {
                        if follows_expr(kind) {
                            break;
                        }

                        self.error_unexpected_str(op_token.span, "binary operator or '?'", kind);
                        return None;
                    }
                };
                let (lbp, rbp) = op_kind.binding_power();
                if lbp < min_bp {
                    break;
                }

                // eat `op`
                self.next();

                let op = BinOp {
                    kind: op_kind,
                    span: op_token.span,
                };
                let rhs = self.parse_expr_binary(rbp)?;
                lhs = ExprAst::new(ExprKind::Binary(BinaryExpr {
                    op,
                    lhs: Node::expr(lhs),
                    rhs,
                }));
            } else {
                let (lbp, rbp) = (3, 2);
                if lbp < min_bp {
                    break;
                }

                self.eat(TokenKind::QMark);

                let then_expr = self.parse_expr_binary(0)?;
                self.consume(TokenKind::Colon)?;
                let else_expr = self.parse_expr_binary(rbp)?;
                lhs = ExprAst::new(ExprKind::Ternary(TernaryExpr {
                    cond: Node::expr(lhs),
                    then_expr,
                    else_expr,
                }));
            };
        }

        Some(Node::expr(lhs))
    }

    pub fn parse_expr_group(&mut self) -> Option<ExprAst> {
        let open_span = self.eat(TokenKind::OParen).span;
        let expr = self.parse_expr()?;
        let close_span = self.consume(TokenKind::CParen)?.span;
        Some(ExprAst::new(ExprKind::Group(GroupExpr {
            expr,
            open_span,
            close_span,
        })))
    }

    pub fn parse_expr_unary(&mut self) -> Option<ExprAst> {
        use TokenKind::*;
        let op_token = self.consume_any(&[Minus, MinusMinus, PlusPlus, Bang, Star, Amps])?;
        let op_kind = match op_token.kind {
            TokenKind::Minus => UnOpKind::Neg,
            TokenKind::MinusMinus => UnOpKind::Dec,
            TokenKind::PlusPlus => UnOpKind::Inc,
            TokenKind::Bang => UnOpKind::Not,
            TokenKind::Star => UnOpKind::Deref,
            TokenKind::Amps => UnOpKind::Ref,
            _ => unreachable!(),
        };

        let op = UnOp {
            kind: op_kind,
            span: op_token.span,
        };
        let expr = self.parse_expr_primary()?;
        Some(ExprAst::new(ExprKind::Unary(UnaryExpr {
            op,
            expr: Node::expr(expr),
        })))
    }

    pub fn parse_expr_primary(&mut self) -> Option<ExprAst> {
        let token = self.peek();
        let expr = match token.kind {
            TokenKind::Name(name) => {
                self.next();
                ExprAst::new(ExprKind::Name(Name::new(name, token.span)))
            }
            TokenKind::Literal(lit) => {
                self.next();
                ExprAst::new(ExprKind::Const(Literal::new(lit, token.span)))
            }
            TokenKind::OParen => self.parse_expr_group()?,
            _ => self.parse_expr_unary()?,
        };

        self.parse_expr_postfix(expr)
    }

    pub fn parse_expr_postfix(&mut self, mut expr: ExprAst) -> Option<ExprAst> {
        loop {
            let op_token = self.peek();
            match op_token.kind {
                TokenKind::OParen => {
                    self.next();
                    let (args, close) =
                        self.parse_any_comma(|p| p.parse_expr(), TokenKind::CParen)?;
                    expr = ExprAst::new(ExprKind::Call(CallExpr {
                        callee: Node::expr(expr),
                        args,
                        close_span: close.span,
                    }));
                }
                TokenKind::OBrack => {
                    self.next();
                    let offset = self.parse_expr()?;
                    let close_span = self.consume(TokenKind::CBrack)?.span;
                    expr = ExprAst::new(ExprKind::Offset(OffsetExpr {
                        base: Node::expr(expr),
                        offset,
                        close_span,
                    }));
                }
                TokenKind::PlusPlus => {
                    let op_span = self.next().span;
                    expr = ExprAst::new(ExprKind::Unary(UnaryExpr {
                        op: UnOp {
                            kind: UnOpKind::PostInc,
                            span: op_span,
                        },
                        expr: Node::expr(expr),
                    }));
                }
                TokenKind::MinusMinus => {
                    let op_span = self.next().span;
                    expr = ExprAst::new(ExprKind::Unary(UnaryExpr {
                        op: UnOp {
                            kind: UnOpKind::PostDec,
                            span: op_span,
                        },
                        expr: Node::expr(expr),
                    }));
                }
                _ => return Some(expr),
            }
        }
    }

    #[inline]
    fn parse_one_or_more_comma<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser) -> Option<T>,
        terminator: TokenKind,
    ) -> Option<(Vec<T>, Token)> {
        let mut list = Vec::new();
        list.push(parse_arg(self)?);
        let term_token = loop {
            let token = self.consume_any(&[terminator, TokenKind::Comma])?;
            if token.kind == terminator {
                break token;
            }
            list.push(parse_arg(self)?);
        };

        Some((list, term_token))
    }

    #[inline]
    fn parse_any_comma<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser) -> Option<T>,
        terminator: TokenKind,
    ) -> Option<(Vec<T>, Token)> {
        if self.peek().kind == terminator {
            return Some((vec![], self.next()));
        }

        let mut list = Vec::new();
        let term_token = loop {
            list.push(parse_arg(self)?);
            let token = self.consume_any(&[terminator, TokenKind::Comma])?;
            if token.kind == terminator {
                break token;
            }
        };

        Some((list, term_token))
    }

    #[inline]
    fn parse_auto_decl(&mut self) -> Option<AutoDecl> {
        let name = self.parse_name()?;
        let value = self.try_parse_const().ok();
        Some(AutoDecl { name, value })
    }

    #[inline]
    fn parse_imm_val(&mut self) -> Option<ImmVal> {
        let Token { kind, span } = self.peek();
        let val = match kind {
            TokenKind::Name(name) => ImmVal::Name(Name::new(name, span)),
            TokenKind::Literal(lit) => ImmVal::Const(Literal::new(lit, span)),
            kind => {
                self.error_unexpected_str(span, "name or constant", kind);
                return None;
            }
        };
        self.next();
        Some(val)
    }

    #[inline]
    fn parse_const(&mut self) -> Option<Literal> {
        self.try_parse_const()
            .inspect_err(|token| self.error_unexpected_str(token.span, "constant", token.kind))
            .ok()
    }

    #[inline]
    fn try_parse_const(&mut self) -> Result<Literal, Token> {
        let token = self.peek();
        if let TokenKind::Literal(lit) = token.kind {
            self.next();
            Ok(Literal::new(lit, token.span))
        } else {
            Err(token)
        }
    }

    #[inline]
    fn parse_name(&mut self) -> Option<Name> {
        let token = self.peek();
        if let TokenKind::Name(name) = token.kind {
            self.next();
            Some(Name::new(name, token.span))
        } else {
            self.error_unexpected_str(token.span, "name", token.kind);
            None
        }
    }
}

#[inline]
fn follows_expr(kind: TokenKind) -> bool {
    use TokenKind::*;
    matches!(
        kind,
        Semi | // goto, semi
        CParen | // if, while, return, group, call, ...
        CBrack | // offset
        Comma | // call
        Colon | // ternary
        // statement start (from switch syntax)
        Keyword(Kw::Auto | Kw::Extrn | Kw::Case | Kw::If | Kw::While | Kw::Switch | Kw::Goto | Kw::Return) |
        Name(_) |
        Literal(_) |
        OBrace |
        Bang
    )
    // Can follow expr, but should be parsed
    // Minus | Amps | OParen | Star | PlusPlus | MinusMinus
}

impl Parser<'_> {
    fn synchronize_def(&mut self) {
        while !self.is_eof() {
            if matches!(self.peek().kind, TokenKind::Name(_))
                && matches!(
                    self.peek2().kind,
                    TokenKind::OParen | TokenKind::OBrack | TokenKind::Literal(_)
                )
            {
                break;
            }
            self.next();
        }
    }

    fn synchronize_stmt(&mut self) {
        while !self.is_eof() {
            let token = self.peek();
            if matches!(
                token.kind,
                TokenKind::Keyword(
                    Kw::Auto | Kw::Extrn | Kw::If | Kw::While | Kw::Switch | Kw::Goto | Kw::Return
                )
            ) {
                break;
            }
            self.next();
        }
    }

    /// Checks if next token kind is 'expected'
    /// Does not consume it or report error
    #[inline]
    fn check(&self, expected: TokenKind) -> Result<(), Token> {
        let token = self.peek();
        if token.kind == expected {
            Ok(())
        } else {
            Err(token)
        }
    }

    #[inline]
    fn check_any(&self, expected: &[TokenKind]) -> Result<Token, Token> {
        let token = self.peek();
        if expected
            .iter()
            .any(|expected_kind| token.kind.eq(expected_kind))
        {
            Ok(token)
        } else {
            Err(token)
        }
    }

    /// Tries to consume next token
    /// Does not report error
    #[inline]
    fn try_consume(&mut self, expected: TokenKind) -> Result<Token, Token> {
        self.check(expected).map(|_| self.next())
    }

    #[inline]
    fn try_consume_any(&mut self, expected: &[TokenKind]) -> Result<Token, Token> {
        self.check_any(expected).inspect(|_| {
            self.next();
        })
    }

    /// Consume the next token if kind is 'expected'
    /// Reports an error on failure
    #[inline]
    fn consume(&mut self, expected: TokenKind) -> Option<Token> {
        self.try_consume(expected)
            .map_err(|found| self.error_unexpected(found.span, expected, found.kind))
            .ok()
    }

    #[inline]
    fn consume_any(&mut self, expected: &[TokenKind]) -> Option<Token> {
        self.try_consume_any(expected)
            .map_err(|found| self.error_unexpected_many(found.span, expected, found.kind))
            .ok()
    }

    // This method is identical to `next` except it checks in debug mode
    fn eat(&mut self, expected: TokenKind) -> Token {
        let token = self.next();
        debug_assert!(token.kind.matches(&expected));
        token
    }

    #[inline]
    fn error_unexpected(&mut self, span: Span, expected: TokenKind, found: TokenKind) {
        self.diag
            .error(
                span,
                format!("unexpected token '{found}', expected '{expected}'"),
            )
            .finish()
    }

    #[inline]
    fn error_unexpected_str(&mut self, span: Span, expected: impl fmt::Display, found: TokenKind) {
        self.diag
            .error(
                span,
                format!("unexpected token '{found}', expected {expected}"),
            )
            .finish()
    }

    #[inline]
    fn error_unexpected_many(&mut self, span: Span, expected: &[TokenKind], found: TokenKind) {
        let mut expected_str = format!(
            "any of {}{}",
            expected[0],
            expected[1..5.min(expected.len())]
                .iter()
                .map(|e| format!(", {e}"))
                .collect::<String>()
        );

        if expected.len() > 5 {
            expected_str.push_str(", ...");
        }

        self.error_unexpected_str(span, expected_str, found);
    }

    #[inline]
    fn peek2(&self) -> Token {
        self.next_tok[1]
    }

    #[inline]
    fn peek(&self) -> Token {
        self.next_tok[0]
    }

    #[inline]
    fn next(&mut self) -> Token {
        let token = self.next_tok[0];
        self.next_tok[0] = self.next_tok[1];
        self.next_tok[1] = self.lexer.next_token();
        token
    }

    #[inline]
    fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }
}
