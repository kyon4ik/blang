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
}

#[derive(Debug)]
pub enum DefKind {
    Vector {
        size: VectorSize,
        list: Vec<ImmVal>,
    },
    Function {
        params: Vec<Name>,
        body: Node<StmtAst>,
    },
}

#[derive(Clone, Copy, Debug)]
pub enum VectorSize {
    Undef,
    Zero,
    Def(Literal),
}

#[derive(Debug)]
pub enum StmtAst {
    Block(Vec<Node<StmtAst>>),
    Cond {
        cond: Node<ExprAst>,
        then_stmt: Node<StmtAst>,
        else_stmt: Option<Node<StmtAst>>,
    },
    While {
        cond: Node<ExprAst>,
        stmt: Node<StmtAst>,
    },
    Switch {
        cond: Node<ExprAst>,
        stmt: Node<StmtAst>,
    },
    Case {
        cnst: Literal,
        stmt: Node<StmtAst>,
    },
    Goto(Name),
    Return(Option<Node<ExprAst>>),
    Semi(Option<Node<ExprAst>>),
    // declarations
    Extrn(Vec<Name>),
    Auto(Vec<AutoDecl>),
    Label {
        name: Name,
        stmt: Node<StmtAst>,
    },
}

#[derive(Debug)]
pub enum ExprAst {
    Name(Name),
    Const(Literal),
    Group(Node<ExprAst>),
    Assign {
        op: AssignOp,
        lhs: Node<ExprAst>,
        rhs: Node<ExprAst>,
    },
    Unary {
        op: UnOp,
        expr: Node<ExprAst>,
    },
    Binary {
        op: BinOp,
        lhs: Node<ExprAst>,
        rhs: Node<ExprAst>,
    },
    Offset {
        base: Node<ExprAst>,
        offset: Node<ExprAst>,
    },
    Ternary {
        cond: Node<ExprAst>,
        then_expr: Node<ExprAst>,
        else_expr: Node<ExprAst>,
    },
    Call {
        callee: Node<ExprAst>,
        args: Vec<Node<ExprAst>>,
    },
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
