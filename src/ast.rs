use bstr::BStr;
pub use node::Node;
use strum_macros::Display;

use crate::diagnostics::Span;
use crate::lexer::interner::InternedStr;
use crate::lexer::token::BinOpKind;

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
    Def(Const),
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
        cnst: Const,
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
    Const(Const),
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
    pub value: Option<Const>,
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

#[derive(Clone, Copy, PartialEq, Eq, Debug, Display)]
pub enum UnOpKind {
    #[strum(to_string = "-")]
    Neg, // -a
    #[strum(to_string = "!")]
    Not, // !a
    #[strum(to_string = "++")]
    Inc, // ++a
    #[strum(to_string = "--")]
    Dec, // --a
    #[strum(to_string = "&")]
    Ref, // &a
    #[strum(to_string = "*")]
    Deref, // *a
    #[strum(to_string = "++")]
    PostInc, // a++
    #[strum(to_string = "--")]
    PostDec, // a--
}

#[derive(Clone, Copy, Debug)]
pub enum ImmVal {
    Const(Const),
    Name(Name),
}

#[derive(Clone, Copy, Debug)]
pub struct Const {
    pub kind: ConstKind,
    pub span: Span,
}

#[derive(Clone, Copy, Debug)]
pub enum ConstKind {
    Number(InternedStr),
    Char(InternedStr),
    String(InternedStr),
}

#[derive(Clone, Copy, Debug)]
pub struct Name {
    pub value: InternedStr,
    pub span: Span,
}

impl Const {
    pub fn new(kind: ConstKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn as_str(&self) -> &BStr {
        match &self.kind {
            ConstKind::Number(s) | ConstKind::String(s) => s.display(),
            ConstKind::Char(s) => s.display(),
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
