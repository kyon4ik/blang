use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};
use crate::lexer::interner::InternedStr;

use super::visit::{ExprVisitor, StmtVisitor};
use super::*;

pub struct NameResolver {
    switch_stack: usize,
    names: HashMap<InternedStr, Span>,
    labels: HashMap<InternedStr, Span>,
    diag: Rc<RefCell<Diagnostics>>,
}

pub struct ValueChecker {
    diag: Rc<RefCell<Diagnostics>>,
}

impl ValueChecker {
    pub fn new(diag: Rc<RefCell<Diagnostics>>) -> Self {
        Self { diag }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        match &def.kind {
            DefKind::Vector { .. } => {}
            DefKind::Function { body, .. } => {
                self.visit_stmt(body);
            }
        }
    }
}

impl NameResolver {
    pub fn new(diag: Rc<RefCell<Diagnostics>>) -> Self {
        Self {
            switch_stack: 0,
            names: HashMap::new(),
            labels: HashMap::new(),
            diag,
        }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.names.clear();
        self.labels.clear();
        assert_eq!(self.switch_stack, 0);
        match &def.kind {
            DefKind::Vector { .. } => {}
            DefKind::Function { params, body } => {
                for param in params {
                    self.declare_name(param);
                }
                self.visit_stmt(body);
            }
        }
    }

    fn declare_name(&mut self, name: &Name) {
        self.names
            .entry(name.value)
            .and_modify(|span| {
                let loc = self.diag.borrow().source_map().locate(span.start).unwrap();
                self.diag.borrow_mut().error(
                    DiagErrorKind::other(format!(
                        "Redefinition of name {}. First defined here {}.",
                        name.value.display(),
                        loc
                    )),
                    name.span,
                )
            })
            .or_insert(name.span);
    }

    fn declare_label(&mut self, name: &Name) {
        self.labels
            .entry(name.value)
            .and_modify(|span| {
                self.diag.borrow_mut().error(
                    DiagErrorKind::other(format!(
                        "Redefinition of label {}. First defined here {:?}.",
                        name.value.display(),
                        self.diag.borrow().source_map().locate_span(*span)
                    )),
                    name.span,
                )
            })
            .or_insert(name.span);
    }
}

impl StmtVisitor for ValueChecker {
    fn visit_auto(&mut self, _decls: &[AutoDecl]) {}

    fn visit_extrn(&mut self, _names: &[Name]) {}
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ValType {
    LValue,
    RValue,
}

impl ExprVisitor for ValueChecker {
    type Value = ValType;

    #[inline]
    fn visit_name(&mut self, _name: &Name) -> Self::Value {
        ValType::LValue
    }

    #[inline]
    fn visit_const(&mut self, _cnst: &Const) -> Self::Value {
        ValType::RValue
    }

    #[inline]
    fn visit_group(&mut self, group: &ExprAst) -> Self::Value {
        self.visit_expr(group);
        ValType::RValue
    }

    #[inline]
    fn visit_assign(&mut self, op: AssignOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value {
        let lhs = self.visit_expr(lhs);
        if lhs != ValType::LValue {
            self.diag.borrow_mut().error(
                DiagErrorKind::other("Left side of assignment must be lvalue"),
                op.span,
            );
        }

        self.visit_expr(rhs);
        ValType::RValue
    }

    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) -> Self::Value {
        let expr = self.visit_expr(expr);

        use UnOpKind::*;
        if matches!(op.kind, Inc | Dec | PostInc | PostDec | Ref) && expr != ValType::LValue {
            self.diag
                .borrow_mut()
                .error(DiagErrorKind::other("Expression must be lvalue"), op.span);
        }

        if op.kind == Deref {
            ValType::LValue
        } else {
            ValType::RValue
        }
    }

    fn visit_binary(&mut self, _op: BinOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value {
        self.visit_expr(lhs);
        self.visit_expr(rhs);
        ValType::RValue
    }

    fn visit_offset(&mut self, base: &ExprAst, offset: &ExprAst) -> Self::Value {
        self.visit_expr(base);
        self.visit_expr(offset);
        ValType::LValue
    }

    fn visit_ternary(
        &mut self,
        cond: &ExprAst,
        then_expr: &ExprAst,
        else_expr: &ExprAst,
    ) -> Self::Value {
        self.visit_expr(cond);
        self.visit_expr(then_expr);
        self.visit_expr(else_expr);
        ValType::RValue
    }

    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]) -> Self::Value {
        self.visit_expr(callee);
        for arg in args {
            self.visit_expr(arg);
        }
        ValType::RValue
    }
}

impl StmtVisitor for NameResolver {
    fn visit_auto(&mut self, decls: &[AutoDecl]) {
        for decl in decls {
            self.declare_name(&decl.name);
        }
    }

    fn visit_extrn(&mut self, names: &[Name]) {
        for name in names {
            self.declare_name(name);
        }
    }

    fn visit_label(&mut self, name: &Name, stmt: &StmtAst) {
        self.declare_label(name);
        self.visit_stmt(stmt);
    }

    // TODO: Add 'case' span
    fn visit_case(&mut self, cnst: &Const, stmt: &StmtAst) {
        if self.switch_stack == 0 {
            self.diag.borrow_mut().error(
                DiagErrorKind::other("Case statement outside of switch."),
                cnst.span,
            )
        }
        self.visit_stmt(stmt);
    }

    fn visit_switch(&mut self, cond: &ExprAst, stmt: &StmtAst) {
        self.switch_stack += 1;
        self.visit_expr(cond);
        self.visit_stmt(stmt);
        self.switch_stack -= 1;
    }
}

impl ExprVisitor for NameResolver {
    type Value = ();

    fn visit_name(&mut self, name: &Name) {
        if !self.names.contains_key(&name.value) {
            self.diag.borrow_mut().error(
                DiagErrorKind::other(format!("Undefined name {}", name.value.display())),
                name.span,
            )
        }
    }

    fn visit_const(&mut self, _cnst: &Const) {}

    fn visit_group(&mut self, group: &ExprAst) -> Self::Value {
        self.visit_expr(group);
    }

    fn visit_assign(&mut self, op: AssignOp, lhs: &ExprAst, rhs: &ExprAst) {
        let _ = op;
        self.visit_expr(lhs);
        self.visit_expr(rhs);
    }

    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) {
        let _ = op;
        self.visit_expr(expr);
    }

    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst) {
        let _ = op;
        self.visit_expr(lhs);
        self.visit_expr(rhs);
    }

    fn visit_offset(&mut self, base: &ExprAst, offset: &ExprAst) {
        self.visit_expr(base);
        self.visit_expr(offset);
    }

    fn visit_ternary(&mut self, cond: &ExprAst, then_expr: &ExprAst, else_expr: &ExprAst) {
        self.visit_expr(cond);
        self.visit_expr(then_expr);
        self.visit_expr(else_expr);
    }

    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]) {
        self.visit_expr(callee);
        for arg in args {
            self.visit_expr(arg);
        }
    }
}
