use std::cell::RefCell;
use std::rc::Rc;

use crate::ast::{
    AutoDecl, Char, Const, ConstKind, DefAst, DefKind, ExprAst, ImmVal, Name, Node, StmtAst, UnOp,
    VectorSize,
};
use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};
use crate::lexer::token::Kw;
use crate::lexer::{BinOp, Lexer, Token, TokenKind};

#[cfg(test)]
mod test;

#[derive(Debug)]
pub struct Parser<'s> {
    lexer: Lexer<'s>,
    diag: Rc<RefCell<Diagnostics>>,
    next_tok: [Token; 2],
}

impl<'s> Parser<'s> {
    pub fn new(mut lexer: Lexer<'s>, diag: Rc<RefCell<Diagnostics>>) -> Self {
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
        self.eat(TokenKind::OParen);
        let params = self.parse_any_comma(|p| p.parse_name(), TokenKind::CParen)?;
        let body = self.parse_stmt()?;
        Some(DefAst {
            name,
            kind: DefKind::Function { params, body },
        })
    }

    pub fn parse_def_vector(&mut self, name: Name) -> Option<DefAst> {
        let size = match self.try_consume(TokenKind::OBrack) {
            Ok(_) => {
                let size = self.try_parse_const();
                self.consume(TokenKind::CBrack)?;
                size.map(VectorSize::Def).unwrap_or(VectorSize::Zero)
            }
            Err(_) => VectorSize::Undef,
        };

        let list = self.parse_any_comma(|p| p.parse_imm_val(), TokenKind::Semi)?;

        Some(DefAst {
            name,
            kind: DefKind::Vector { size, list },
        })
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
        self.eat(TokenKind::Keyword(Kw::Auto));
        let list = self.parse_one_or_more_comma(|p| p.parse_auto_decl(), TokenKind::Semi)?;
        Some(StmtAst::Auto(list))
    }

    pub fn parse_stmt_extrn(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::Extrn));
        let list = self.parse_one_or_more_comma(|p| p.parse_name(), TokenKind::Semi)?;
        Some(StmtAst::Extrn(list))
    }

    pub fn parse_stmt_case(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::Case));
        let cnst = self.parse_const()?;
        self.consume(TokenKind::Colon)?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::Case { cnst, stmt })
    }

    pub fn parse_stmt_cond(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::If));

        self.consume(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.consume(TokenKind::CParen)?;

        let then_stmt = self.parse_stmt()?;
        let else_stmt = match self.try_consume(TokenKind::Keyword(Kw::Else)) {
            Ok(_) => Some(self.parse_stmt()?),
            Err(_) => None,
        };

        Some(StmtAst::Cond {
            cond,
            then_stmt,
            else_stmt,
        })
    }

    pub fn parse_stmt_while(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::While));

        self.consume(TokenKind::OParen)?;
        let cond = self.parse_expr()?;
        self.consume(TokenKind::CParen)?;

        let stmt = self.parse_stmt()?;
        Some(StmtAst::While { cond, stmt })
    }

    pub fn parse_stmt_switch(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::Switch));
        let cond = self.parse_expr()?;
        let stmt = self.parse_stmt()?;
        Some(StmtAst::Switch { cond, stmt })
    }

    pub fn parse_stmt_goto(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::Goto));
        let expr = self.parse_expr()?;
        self.consume(TokenKind::Semi)?;
        Some(StmtAst::Goto(expr))
    }

    pub fn parse_stmt_return(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::Keyword(Kw::Return));

        let expr = match self.try_consume(TokenKind::OParen) {
            Ok(_) => {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::CParen)?;
                Some(expr)
            }
            Err(_) => None,
        };
        self.consume(TokenKind::Semi)?;
        Some(StmtAst::Return(expr))
    }

    pub fn parse_stmt_label(&mut self, name: Name) -> Option<StmtAst> {
        self.eat(TokenKind::dummy_name());
        self.eat(TokenKind::Colon);
        let stmt = self.parse_stmt()?;
        Some(StmtAst::Label { name, stmt })
    }

    pub fn parse_stmt_block(&mut self) -> Option<StmtAst> {
        self.eat(TokenKind::OBrace);

        let mut stmts = Vec::new();
        while self.peek().kind != TokenKind::CBrace {
            stmts.push(self.parse_stmt()?);
        }
        self.eat(TokenKind::CBrace);

        Some(StmtAst::Block(stmts))
    }

    pub fn parse_stmt_semi(&mut self) -> Option<StmtAst> {
        let expr = match self.try_consume(TokenKind::Semi) {
            Ok(_) => None,
            Err(_) => {
                let expr = self.parse_expr()?;
                self.consume(TokenKind::Semi)?;
                Some(expr)
            }
        };
        Some(StmtAst::Semi(expr))
    }

    pub fn parse_expr(&mut self) -> Option<Node<ExprAst>> {
        self.parse_expr_assign()
    }

    pub fn parse_expr_assign(&mut self) -> Option<Node<ExprAst>> {
        let lhs = self.parse_expr_binary(0)?;

        // FIXME: you consume_any smh?
        let op_token = self.peek();
        let op = match op_token.kind {
            TokenKind::Assign(bin_op) => Some(bin_op),
            TokenKind::Eq => None,
            kind => {
                if follows_expr(kind) {
                    return Some(lhs);
                }

                self.error(
                    DiagErrorKind::unexpected("token", "operator or ;", format!("{kind}")),
                    op_token.span,
                );
                return None;
            }
        };

        // eat `op`
        self.next();

        let rhs = self.parse_expr_assign()?;
        Some(Node::expr(ExprAst::Assign { op, lhs, rhs }))
    }

    #[inline]
    fn parse_expr_binary(&mut self, min_bp: u8) -> Option<Node<ExprAst>> {
        let mut lhs = self.parse_expr_primary()?;

        loop {
            let op_token = self.peek();

            // FIXME: Hack for ternary
            if op_token.kind != TokenKind::QMark {
                let op = match op_token.kind {
                    TokenKind::Plus => BinOp::Add,
                    TokenKind::Star => BinOp::Mul,
                    TokenKind::Minus => BinOp::Sub,
                    TokenKind::Slash => BinOp::Div,
                    TokenKind::Percent => BinOp::Rem,
                    TokenKind::Amps => BinOp::And,
                    TokenKind::Pipe => BinOp::Or,
                    TokenKind::LtLt => BinOp::Shl,
                    TokenKind::GtGt => BinOp::Shr,
                    TokenKind::EqEq => BinOp::Eq,
                    TokenKind::BangEq => BinOp::Neq,
                    TokenKind::LtEq => BinOp::LtEq,
                    TokenKind::GtEq => BinOp::GtEq,
                    TokenKind::Lt => BinOp::Lt,
                    TokenKind::Gt => BinOp::Gt,
                    // assign expr next
                    TokenKind::Assign(_) | TokenKind::Eq => break,
                    kind => {
                        if follows_expr(kind) {
                            break;
                        }

                        self.error(
                            DiagErrorKind::unexpected("token", "operator or ;", format!("{kind}")),
                            op_token.span,
                        );
                        return None;
                    }
                };
                let (lbp, rbp) = op.binding_power();
                if lbp < min_bp {
                    break;
                }

                // eat `op`
                self.next();

                let rhs = self.parse_expr_binary(rbp)?;
                lhs = ExprAst::Binary {
                    op,
                    lhs: Node::expr(lhs),
                    rhs,
                };
            } else {
                let (lbp, rbp) = (3, 2);
                if lbp < min_bp {
                    break;
                }

                self.eat(TokenKind::QMark);

                let then_expr = self.parse_expr_binary(0)?;
                self.consume(TokenKind::Colon)?;
                let else_expr = self.parse_expr_binary(rbp)?;
                lhs = ExprAst::Ternary {
                    cond: Node::expr(lhs),
                    then_expr,
                    else_expr,
                }
            };
        }

        Some(Node::expr(lhs))
    }

    pub fn parse_expr_group(&mut self) -> Option<ExprAst> {
        self.eat(TokenKind::OParen);
        let expr = self.parse_expr()?;
        self.consume(TokenKind::CParen)?;
        Some(ExprAst::Group(expr))
    }

    pub fn parse_expr_unary(&mut self) -> Option<ExprAst> {
        use TokenKind::*;
        let op_token = self.consume_any(&[Minus, MinusMinus, PlusPlus, Bang, Star, Amps])?;
        let op = match op_token.kind {
            TokenKind::Minus => UnOp::Neg,
            TokenKind::MinusMinus => UnOp::Dec,
            TokenKind::PlusPlus => UnOp::Inc,
            TokenKind::Bang => UnOp::Not,
            TokenKind::Star => UnOp::Deref,
            TokenKind::Amps => UnOp::Ref,
            _ => unreachable!(),
        };

        let expr = self.parse_expr_primary()?;
        Some(ExprAst::Unary {
            op,
            expr: Node::expr(expr),
        })
    }

    pub fn parse_expr_primary(&mut self) -> Option<ExprAst> {
        let token = self.peek();
        let expr = match token.kind {
            TokenKind::Name(name) => {
                self.next();
                ExprAst::Name(Name::new(name, token.span))
            }
            TokenKind::Number(num) => {
                self.next();
                ExprAst::Const(Const::new(ConstKind::Number(num), token.span))
            }
            TokenKind::Char(char) => {
                self.next();
                ExprAst::Const(Const::new(ConstKind::Char(Char(char)), token.span))
            }
            TokenKind::String(str) => {
                self.next();
                ExprAst::Const(Const::new(ConstKind::String(str), token.span))
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
                    let args = self.parse_any_comma(|p| p.parse_expr(), TokenKind::CParen)?;
                    expr = ExprAst::Call {
                        callee: Node::expr(expr),
                        args,
                    };
                }
                TokenKind::OBrack => {
                    self.next();
                    let offset = self.parse_expr()?;
                    self.consume(TokenKind::CBrack)?;
                    expr = ExprAst::Offset {
                        base: Node::expr(expr),
                        offset,
                    };
                }
                TokenKind::PlusPlus => {
                    self.next();
                    expr = ExprAst::Unary {
                        op: UnOp::PostInc,
                        expr: Node::expr(expr),
                    };
                }
                TokenKind::MinusMinus => {
                    self.next();
                    expr = ExprAst::Unary {
                        op: UnOp::PostDec,
                        expr: Node::expr(expr),
                    };
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
    ) -> Option<Vec<T>> {
        let mut list = Vec::new();
        list.push(parse_arg(self)?);
        while self.consume_any(&[terminator, TokenKind::Comma])?.kind != terminator {
            list.push(parse_arg(self)?);
        }

        Some(list)
    }

    #[inline]
    fn parse_any_comma<T>(
        &mut self,
        parse_arg: impl Fn(&mut Parser) -> Option<T>,
        terminator: TokenKind,
    ) -> Option<Vec<T>> {
        if self.peek().kind == terminator {
            self.next();
            return Some(vec![]);
        }

        let mut list = Vec::new();
        loop {
            list.push(parse_arg(self)?);
            if self.consume_any(&[terminator, TokenKind::Comma])?.kind == terminator {
                break;
            }
        }

        Some(list)
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
            TokenKind::Number(num) => ImmVal::Const(Const::new(ConstKind::Number(num), span)),
            TokenKind::Char(char) => ImmVal::Const(Const::new(ConstKind::Char(Char(char)), span)),
            TokenKind::String(str) => ImmVal::Const(Const::new(ConstKind::String(str), span)),
            kind => {
                self.error(
                    DiagErrorKind::unexpected("token", "name or const", format!("{kind}")),
                    span,
                );
                return None;
            }
        };
        self.next();
        Some(val)
    }

    #[inline]
    fn parse_const(&mut self) -> Option<Const> {
        self.try_parse_const()
            .inspect_err(|token| {
                self.error(
                    DiagErrorKind::unexpected("token", "const", format!("{}", token.kind)),
                    token.span,
                )
            })
            .ok()
    }

    #[inline]
    fn try_parse_const(&mut self) -> Result<Const, Token> {
        let token = self.peek();
        let kind = match token.kind {
            TokenKind::Number(num) => ConstKind::Number(num),
            TokenKind::Char(char) => ConstKind::Char(Char(char)),
            TokenKind::String(str) => ConstKind::String(str),
            _ => return Err(token),
        };
        self.next();
        Ok(Const {
            kind,
            span: token.span,
        })
    }

    #[inline]
    fn parse_name(&mut self) -> Option<Name> {
        let token = self.peek();
        if let TokenKind::Name(name) = token.kind {
            self.next();
            Some(Name::new(name, token.span))
        } else {
            self.error(
                DiagErrorKind::unexpected("token", "Name", format!("{}", token.kind)),
                token.span,
            );
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
        Number(_) |
        Char(_) |
        String(_) |
        OBrace |
        OParen |
        Star |
        PlusPlus |
        MinusMinus |
        Minus |
        Bang |
        Amps
    )
}

impl Parser<'_> {
    fn synchronize_def(&mut self) {
        while !self.is_eof() {
            if matches!(self.peek().kind, TokenKind::Name(_))
                && matches!(
                    self.peek2().kind,
                    TokenKind::OParen
                        | TokenKind::OBrack
                        | TokenKind::String(_)
                        | TokenKind::Char(_)
                        | TokenKind::Number(_)
                        | TokenKind::Name(_)
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
    fn try_consume(&mut self, expected: TokenKind) -> Result<(), Token> {
        self.check(expected).inspect(|_| {
            self.next();
        })
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
    fn consume(&mut self, expected: TokenKind) -> Option<()> {
        self.try_consume(expected)
            .map_err(|found| {
                self.error(
                    DiagErrorKind::unexpected(
                        "token",
                        format!("{expected}"),
                        format!("{}", found.kind),
                    ),
                    found.span,
                )
            })
            .ok()
    }

    #[inline]
    fn consume_any(&mut self, expected: &[TokenKind]) -> Option<Token> {
        self.try_consume_any(expected)
            .map_err(|found| {
                self.error(
                    DiagErrorKind::unexpected(
                        "token",
                        format!(
                            "any of {}{}",
                            expected[0],
                            expected[1..]
                                .iter()
                                .map(|e| format!(", {e}"))
                                .collect::<String>()
                        ),
                        format!("{}", found.kind),
                    ),
                    found.span,
                )
            })
            .ok()
    }

    // This method is identical to `next` except it checks in debug mode
    fn eat(&mut self, expected: TokenKind) {
        let token = self.next();
        debug_assert!(token.kind.matches(&expected));
    }

    #[inline]
    fn error(&mut self, kind: DiagErrorKind, span: Span) {
        self.diag.borrow_mut().error(kind, span)
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
