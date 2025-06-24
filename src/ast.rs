use std::ops::{Deref, DerefMut};
use std::sync::LazyLock;

use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::lexer::interner::InternedStr;
use crate::lexer::token::{MAX_CHAR_LEN, MAX_NAME_LEN, MAX_NUMBER_LEN};

static STMT_NODE_ARENA: LazyLock<Arena<StmtAst>> = LazyLock::new(Arena::new);
static EXPR_NODE_ARENA: LazyLock<Arena<ExprAst>> = LazyLock::new(Arena::new);

pub struct Node<T: 'static> {
    inner: &'static mut T,
}

impl Node<ExprAst> {
    pub fn expr(ast: ExprAst) -> Self {
        Self {
            inner: EXPR_NODE_ARENA.alloc(ast),
        }
    }
}

impl Node<StmtAst> {
    pub fn expr(ast: StmtAst) -> Self {
        Self {
            inner: STMT_NODE_ARENA.alloc(ast),
        }
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.inner
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner
    }
}

pub struct DefAst {
    pub name: Name,
    pub kind: DefKind,
}

pub enum DefKind {
    Vector {
        size: Option<Const>,
        list: Vec<ImmVal>,
    },
    Function {
        params: Vec<Name>,
        body: Node<StmtAst>,
    },
}

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
    Goto(Node<ExprAst>),
    Return(Option<Node<ExprAst>>),
    Semi(Option<Node<ExprAst>>),
    // declarations
    Extrn(Vec<Name>),
    Auto(Vec<AutoDecl>),
    Label(Name),
}

pub enum ExprAst {
    ImmVal(ImmVal),
    Group(Node<ExprAst>),
    Assign {
        op: Option<BinaryOp>,
        lvalue: Node<ExprAst>,
        rvalue: Node<ExprAst>,
    },
    Unary {
        op: UnaryOp,
        expr: Node<ExprAst>,
    },
    Binary {
        op: BinaryOp,
        left: Node<ExprAst>,
        right: Node<ExprAst>,
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
pub enum UnaryOp {
    Neg,     // -a
    Not,     // !a
    Inc,     // ++a
    Dec,     // --a
    Ref,     // &a
    Deref,   // *a
    PostInc, // a++
    PostDec, // a--
}

#[derive(Clone, Copy, Debug)]
pub enum BinaryOp {
    Or,   // |
    And,  // &
    Eq,   // ==
    Neq,  // !=
    Lt,   // <
    LtEq, // <=
    Gt,   // >
    GtEq, // >=
    Shl,  // <<
    Shr,  // >>
    Add,  // +
    Sub,  // -
    Rem,  // %
    Mul,  // *
    Div,  // /
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
    Number([u8; MAX_NUMBER_LEN]),
    Char([u8; MAX_CHAR_LEN]),
    String(InternedStr),
}

#[derive(Clone, Copy, Debug)]
pub struct Name {
    pub lexeme: [u8; MAX_NAME_LEN],
    pub span: Span,
}
