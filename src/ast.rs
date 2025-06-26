use std::ops::{Deref, DerefMut};
use std::sync::LazyLock;

use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::lexer::BinOp;
use crate::lexer::interner::InternedStr;
use crate::lexer::token::{MAX_CHAR_LEN, MAX_NAME_LEN, MAX_NUMBER_LEN};

pub mod visit;

static STMT_NODE_ARENA: LazyLock<Arena<StmtAst>> = LazyLock::new(Arena::new);
static EXPR_NODE_ARENA: LazyLock<Arena<ExprAst>> = LazyLock::new(Arena::new);

#[derive(Debug)]
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
    pub fn stmt(ast: StmtAst) -> Self {
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

#[derive(Debug)]
pub struct DefAst {
    pub name: Name,
    pub kind: DefKind,
}

#[derive(Debug)]
pub enum DefKind {
    Vector {
        // FIXME: size is optional and const also
        size: Option<Const>,
        list: Vec<ImmVal>,
    },
    Function {
        params: Vec<Name>,
        body: Node<StmtAst>,
    },
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
    Goto(Node<ExprAst>),
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
        op: Option<BinOp>,
        lvalue: Node<ExprAst>,
        rvalue: Node<ExprAst>,
    },
    Unary {
        op: UnOp,
        expr: Node<ExprAst>,
    },
    Binary {
        op: BinOp,
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
pub enum UnOp {
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

impl Const {
    pub fn new(kind: ConstKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl Name {
    pub fn new(lexeme: [u8; MAX_NAME_LEN], span: Span) -> Self {
        Self { lexeme, span }
    }
}
