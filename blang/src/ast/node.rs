use std::ops::{Deref, DerefMut};
use std::sync::LazyLock;

use crate::arena::Arena;

use super::{ExprAst, StmtAst};

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
