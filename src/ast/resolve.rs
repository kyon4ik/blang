use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostics::{Diagnostics, Span};
use crate::lexer::interner::InternedStr;

use super::visit::{ExprVisitor, StmtVisitor};
use super::*;

pub struct NameResolver {
    switch_stack: usize,
    globals: HashMap<InternedStr, Span>,
    names: HashMap<InternedStr, Span>,
    used_labels: HashMap<InternedStr, Span>,
    labels: HashMap<InternedStr, Span>,
    diag: Rc<Diagnostics>,
}

pub struct ValueChecker {
    diag: Rc<Diagnostics>,
}

impl ValueChecker {
    pub fn new(diag: Rc<Diagnostics>) -> Self {
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
    pub fn new(diag: Rc<Diagnostics>) -> Self {
        Self {
            switch_stack: 0,
            globals: HashMap::new(),
            names: HashMap::new(),
            used_labels: HashMap::new(),
            labels: HashMap::new(),
            diag,
        }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.names.clear();
        self.labels.clear();
        assert_eq!(self.switch_stack, 0);
        self.declare_global(&def.name);
        match &def.kind {
            DefKind::Vector { list, .. } => {
                for ival in list {
                    if let ImmVal::Name(name) = ival {
                        self.use_global(name);
                    }
                }
            }
            DefKind::Function { params, body } => {
                for param in params {
                    self.declare_name(param);
                }
                self.visit_stmt(body);
            }
        }
        for (value, span) in self.used_labels.drain() {
            self.diag
                .error(span, format!("undefined label '{}'.", value.display()))
                .finish();
        }
    }

    fn use_label(&mut self, name: &Name) {
        // FIXME: maybe used in several places
        self.used_labels.insert(name.value, name.span);
    }

    fn use_global(&mut self, name: &Name) {
        self.globals.entry(name.value).or_insert(Span::empty());
    }

    fn declare_global(&mut self, name: &Name) {
        self.globals
            .entry(name.value)
            .and_modify(|span| {
                if *span != Span::empty() {
                    self.diag
                        .error(
                            name.span,
                            format!("redefinition of global name {}", name.value.display()),
                        )
                        .add_label(*span, "first defined here")
                        .finish();
                } else {
                    *span = name.span;
                }
            })
            .or_insert(name.span);
    }

    fn declare_name(&mut self, name: &Name) {
        self.names
            .entry(name.value)
            .and_modify(|span| {
                self.diag
                    .error(
                        name.span,
                        format!("redefinition of name {}", name.value.display()),
                    )
                    .add_label(*span, "first defined here")
                    .finish();
            })
            .or_insert(name.span);
    }

    fn declare_label(&mut self, name: &Name) {
        self.used_labels.remove(&name.value);
        self.labels
            .entry(name.value)
            .and_modify(|span| {
                self.diag
                    .error(
                        name.span,
                        format!("redefinition of label {}", name.value.display()),
                    )
                    .add_label(*span, "first defined here")
                    .finish();
            })
            .or_insert(name.span);
    }
}

impl StmtVisitor for ValueChecker {
    fn visit_auto(&mut self, _decls: &[AutoDecl]) {}

    fn visit_extrn(&mut self, _names: &[Name]) {}

    fn visit_goto(&mut self, _label: &Name) {}
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
            self.diag
                .error(op.span, "left operand of assignment must be lvalue")
                .finish();
        }

        self.visit_expr(rhs);
        ValType::RValue
    }

    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) -> Self::Value {
        let expr = self.visit_expr(expr);

        use UnOpKind::*;
        if matches!(op.kind, Inc | Dec | PostInc | PostDec | Ref) && expr != ValType::LValue {
            self.diag
                .error(op.span, format!("'{}' expects lvalue as operand", op.kind))
                .finish();
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
            self.use_global(name);
            self.declare_name(name);
        }
    }

    fn visit_goto(&mut self, label: &Name) {
        self.use_label(label);
    }

    fn visit_label(&mut self, name: &Name, stmt: &StmtAst) {
        self.declare_label(name);
        self.visit_stmt(stmt);
    }

    // TODO: Add 'case' span
    fn visit_case(&mut self, cnst: &Const, stmt: &StmtAst) {
        if self.switch_stack == 0 {
            self.diag
                .error(cnst.span, "case statement outside of switch.")
                .finish();
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
            self.diag
                .error(
                    name.span,
                    format!("undefined name {}", name.value.display()),
                )
                .finish()
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
