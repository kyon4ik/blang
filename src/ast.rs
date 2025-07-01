use bstr::BStr;
pub use node::Node;
use strum_macros::{Display, IntoStaticStr};

use crate::diagnostics::Span;
use crate::lexer;
use crate::lexer::interner::InternedStr;
use crate::lexer::token::{BinOpKind, LiteralKind};

pub mod node;
pub mod print;
pub mod resolve;
pub mod visit;

#[derive(Debug)]
pub struct DefAst {
    pub name: Name,
    pub kind: DefKind,
    pub span: Span,
}

impl DefAst {
    pub fn new(name: Name, kind: DefKind) -> Self {
        let span = if let Some(def_span) = kind.span() {
            name.span.unite(def_span)
        } else {
            name.span
        };
        Self { name, kind, span }
    }
}

impl ImmVal {
    pub fn span(&self) -> Span {
        match self {
            ImmVal::Const(lit) => lit.span,
            ImmVal::Name(name) => name.span,
        }
    }
}

impl DefKind {
    pub fn span(&self) -> Option<Span> {
        match self {
            DefKind::Vector { size, list } => {
                let size_span = match size {
                    VecSize::Undef => None,
                    VecSize::Empty(span) => Some(span),
                    VecSize::Def { span, .. } => Some(span),
                };
                match (size_span, list.last().map(|iv| iv.span())) {
                    (Some(size_span), Some(list_span)) => Some(size_span.unite(list_span)),
                    (Some(size_span), None) => Some(*size_span),
                    (None, Some(list_span)) => Some(list_span),
                    _ => None,
                }
            }
            DefKind::Function { params, body } => Some(params.open_span.unite(body.span)),
        }
    }
}

#[derive(Debug)]
pub struct FuncParams {
    pub params: Vec<Name>,
    pub open_span: Span,
    pub close_span: Span,
}

#[derive(Debug)]
pub enum DefKind {
    Vector {
        size: VecSize,
        list: Vec<ImmVal>,
    },
    Function {
        params: FuncParams,
        body: Node<StmtAst>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum VecSize {
    Undef,
    Empty(Span),
    Def { lit: Literal, span: Span },
}

#[derive(Debug)]
pub struct StmtAst {
    pub kind: StmtKind,
    pub span: Span,
}

impl StmtAst {
    pub fn new(kind: StmtKind) -> Self {
        let span = kind.span();
        Self { kind, span }
    }
}

impl StmtKind {
    pub fn span(&self) -> Span {
        match self {
            StmtKind::Block(block) => block.open_span.unite(block.close_span),
            StmtKind::Cond(cond) => cond.if_span.unite(
                cond.else_stmt
                    .as_ref()
                    .map_or(cond.then_stmt.span, |es| es.span),
            ),
            StmtKind::While(while_) => while_.while_span.unite(while_.stmt.span),
            StmtKind::Switch(switch) => switch.switch_span.unite(switch.stmt.span),
            StmtKind::Case(case) => case.case_span.unite(case.stmt.span),
            StmtKind::Goto(goto) => goto.goto_span.unite(goto.semi_span),
            StmtKind::Return(return_) => return_.return_span.unite(return_.semi_span),
            StmtKind::Semi(semi) => semi
                .expr
                .as_ref()
                .map_or(semi.semi_span, |e| e.span.unite(semi.semi_span)),
            StmtKind::Extrn(extrn) => extrn.extrn_span.unite(extrn.semi_span),
            StmtKind::Auto(auto) => auto.auto_span.unite(auto.semi_span),
            StmtKind::Label(label) => label.name.span.unite(label.stmt.span),
        }
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    pub stmts: Vec<Node<StmtAst>>,
    pub open_span: Span,
    pub close_span: Span,
}

#[derive(Debug)]
pub struct CondStmt {
    pub cond: Node<ExprAst>,
    pub then_stmt: Node<StmtAst>,
    pub else_stmt: Option<Node<StmtAst>>,
    pub if_span: Span,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub cond: Node<ExprAst>,
    pub stmt: Node<StmtAst>,
    pub while_span: Span,
}

#[derive(Debug)]
pub struct SwitchStmt {
    pub cond: Node<ExprAst>,
    pub stmt: Node<StmtAst>,
    pub switch_span: Span,
}

#[derive(Debug)]
pub struct CaseStmt {
    pub cnst: Literal,
    pub stmt: Node<StmtAst>,
    pub case_span: Span,
}

#[derive(Debug)]
pub struct ExtrnStmt {
    pub names: Vec<Name>,
    pub extrn_span: Span,
    pub semi_span: Span,
}

#[derive(Debug)]
pub struct AutoStmt {
    pub decls: Vec<AutoDecl>,
    pub auto_span: Span,
    pub semi_span: Span,
}

#[derive(Debug)]
pub struct LabelStmt {
    pub name: Name,
    pub stmt: Node<StmtAst>,
}

#[derive(Debug)]
pub struct GotoStmt {
    pub label: Name,
    pub goto_span: Span,
    pub semi_span: Span,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Option<Node<ExprAst>>,
    pub return_span: Span,
    pub semi_span: Span,
}

#[derive(Debug)]
pub struct SemiStmt {
    pub expr: Option<Node<ExprAst>>,
    pub semi_span: Span,
}

#[derive(Debug)]
pub enum StmtKind {
    Block(BlockStmt),
    Cond(CondStmt),
    While(WhileStmt),
    Switch(SwitchStmt),
    Case(CaseStmt),
    Goto(GotoStmt),
    Return(ReturnStmt),
    Semi(SemiStmt),
    // declarations
    Extrn(ExtrnStmt),
    Auto(AutoStmt),
    Label(LabelStmt),
}

#[derive(Debug)]
pub struct ExprAst {
    pub kind: ExprKind,
    pub span: Span,
}

impl ExprAst {
    pub fn new(kind: ExprKind) -> Self {
        let span = kind.span();
        Self { kind, span }
    }
}

impl ExprKind {
    pub fn span(&self) -> Span {
        match self {
            ExprKind::Name(name) => name.span,
            ExprKind::Const(literal) => literal.span,
            ExprKind::Group(group) => group.open_span.unite(group.close_span),
            ExprKind::Assign(assign) => assign.lhs.span.unite(assign.rhs.span),
            ExprKind::Unary(unary) => unary.op.span.unite(unary.expr.span),
            ExprKind::Binary(binary) => binary.lhs.span.unite(binary.rhs.span),
            ExprKind::Offset(offset) => offset.base.span.unite(offset.close_span),
            ExprKind::Ternary(ternary) => ternary.cond.span.unite(ternary.else_expr.span),
            ExprKind::Call(call) => call.callee.span.unite(call.close_span),
        }
    }
}

#[derive(Debug)]
pub struct GroupExpr {
    pub expr: Node<ExprAst>,
    pub open_span: Span,
    pub close_span: Span,
}

#[derive(Debug)]
pub struct AssignExpr {
    pub op: AssignOp,
    pub lhs: Node<ExprAst>,
    pub rhs: Node<ExprAst>,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub op: UnOp,
    pub expr: Node<ExprAst>,
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub op: BinOp,
    pub lhs: Node<ExprAst>,
    pub rhs: Node<ExprAst>,
}

#[derive(Debug)]
pub struct OffsetExpr {
    pub base: Node<ExprAst>,
    pub offset: Node<ExprAst>,
    pub close_span: Span,
}

#[derive(Debug)]
pub struct TernaryExpr {
    pub cond: Node<ExprAst>,
    pub then_expr: Node<ExprAst>,
    pub else_expr: Node<ExprAst>,
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Node<ExprAst>,
    pub args: Vec<Node<ExprAst>>,
    pub close_span: Span,
}

#[derive(Debug)]
pub enum ExprKind {
    Name(Name),
    Const(Literal),
    Group(GroupExpr),
    Assign(AssignExpr),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Offset(OffsetExpr),
    Ternary(TernaryExpr),
    Call(CallExpr),
}

#[derive(Clone, Copy, Debug)]
pub struct AutoDecl {
    pub name: Name,
    pub value: Option<Literal>,
}

#[derive(Clone, Copy, Debug)]
pub struct AssignOp {
    pub kind: Option<BinOpKind>,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct BinOp {
    pub kind: BinOpKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct UnOp {
    pub kind: UnOpKind,
    pub span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, IntoStaticStr, Display)]
pub enum UnOpKind {
    #[strum(serialize = "-")]
    Neg, // -a
    #[strum(serialize = "!")]
    Not, // !a
    #[strum(serialize = "++()")]
    Inc, // ++a
    #[strum(serialize = "--()")]
    Dec, // --a
    #[strum(serialize = "&")]
    Ref, // &a
    #[strum(serialize = "*")]
    Deref, // *a
    #[strum(serialize = "()++")]
    PostInc, // a++
    #[strum(serialize = "()--")]
    PostDec, // a--
}

#[derive(Clone, Copy, Debug)]
pub enum ImmVal {
    Const(Literal),
    Name(Name),
}

#[derive(Clone, Copy, Debug)]
pub struct Literal {
    pub kind: LiteralKind,
    pub value: InternedStr,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub struct Name {
    pub value: InternedStr,
    pub span: Span,
}

impl Literal {
    pub fn new(lit: lexer::token::Literal, span: Span) -> Self {
        Self {
            kind: lit.kind,
            value: lit.value,
            span,
        }
    }
}

impl Name {
    pub fn new(value: InternedStr, span: Span) -> Self {
        Self { value, span }
    }

    pub fn as_str(&self) -> &BStr {
        self.value.display()
    }
}
