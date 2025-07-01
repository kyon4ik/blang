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
                for param in &params.params {
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
    fn visit_auto(&mut self, _auto: &AutoStmt) {}

    fn visit_extrn(&mut self, _extrn: &ExtrnStmt) {}

    fn visit_goto(&mut self, _goto: &GotoStmt) {}
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
    fn visit_const(&mut self, _cnst: &Literal) -> Self::Value {
        ValType::RValue
    }

    #[inline]
    fn visit_group(&mut self, group: &GroupExpr) -> Self::Value {
        self.visit_expr(&group.expr);
        ValType::RValue
    }

    #[inline]
    fn visit_assign(&mut self, assign: &AssignExpr) -> Self::Value {
        let lhs = self.visit_expr(&assign.lhs);
        if lhs != ValType::LValue {
            self.diag
                .error(assign.op.span, "left operand of assignment must be lvalue")
                .add_label(assign.lhs.span, "found rvalue")
                .finish();
        }

        self.visit_expr(&assign.rhs);
        ValType::RValue
    }

    fn visit_unary(&mut self, unary: &UnaryExpr) -> Self::Value {
        let expr = self.visit_expr(&unary.expr);

        use UnOpKind::*;
        if matches!(unary.op.kind, Inc | Dec | PostInc | PostDec | Ref) && expr != ValType::LValue {
            self.diag
                .error(
                    unary.op.span,
                    format!("'{}' expects lvalue as operand", unary.op.kind),
                )
                .add_label(unary.expr.span, "found rvalue")
                .finish();
        }

        if unary.op.kind == Deref {
            ValType::LValue
        } else {
            ValType::RValue
        }
    }

    fn visit_binary(&mut self, binary: &BinaryExpr) -> Self::Value {
        self.visit_expr(&binary.lhs);
        self.visit_expr(&binary.rhs);
        ValType::RValue
    }

    fn visit_offset(&mut self, offset: &OffsetExpr) -> Self::Value {
        self.visit_expr(&offset.base);
        self.visit_expr(&offset.offset);
        ValType::LValue
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) -> Self::Value {
        self.visit_expr(&ternary.cond);
        self.visit_expr(&ternary.then_expr);
        self.visit_expr(&ternary.else_expr);
        ValType::RValue
    }

    fn visit_call(&mut self, call: &CallExpr) -> Self::Value {
        self.visit_expr(&call.callee);
        for arg in &call.args {
            self.visit_expr(arg);
        }
        ValType::RValue
    }
}

impl StmtVisitor for NameResolver {
    fn visit_auto(&mut self, auto: &AutoStmt) {
        for decl in &auto.decls {
            self.declare_name(&decl.name);
        }
    }

    fn visit_extrn(&mut self, extrn: &ExtrnStmt) {
        for name in &extrn.names {
            self.use_global(name);
            self.declare_name(name);
        }
    }

    fn visit_goto(&mut self, goto: &GotoStmt) {
        self.use_label(&goto.label);
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        self.declare_label(&label.name);
        self.visit_stmt(&label.stmt);
    }

    // TODO: Add 'case' span
    fn visit_case(&mut self, case: &CaseStmt) {
        if self.switch_stack == 0 {
            self.diag
                .error(case.cnst.span, "case statement outside of switch.")
                .finish();
        }
        self.visit_stmt(&case.stmt);
    }

    fn visit_switch(&mut self, switch: &SwitchStmt) {
        self.switch_stack += 1;
        self.visit_expr(&switch.cond);
        self.visit_stmt(&switch.stmt);
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

    fn visit_const(&mut self, _cnst: &Literal) {}

    fn visit_group(&mut self, group: &GroupExpr) -> Self::Value {
        self.visit_expr(&group.expr);
    }

    fn visit_assign(&mut self, assign: &AssignExpr) {
        self.visit_expr(&assign.lhs);
        self.visit_expr(&assign.rhs);
    }

    fn visit_unary(&mut self, unary: &UnaryExpr) {
        self.visit_expr(&unary.expr);
    }

    fn visit_binary(&mut self, binary: &BinaryExpr) {
        self.visit_expr(&binary.lhs);
        self.visit_expr(&binary.rhs);
    }

    fn visit_offset(&mut self, offset: &OffsetExpr) {
        self.visit_expr(&offset.base);
        self.visit_expr(&offset.offset);
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) {
        self.visit_expr(&ternary.cond);
        self.visit_expr(&ternary.then_expr);
        self.visit_expr(&ternary.else_expr);
    }

    fn visit_call(&mut self, call: &CallExpr) {
        self.visit_expr(&call.callee);
        for arg in &call.args {
            self.visit_expr(arg);
        }
    }
}
