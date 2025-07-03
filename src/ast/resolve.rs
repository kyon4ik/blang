use std::rc::Rc;

use rustc_hash::FxHashMap;

use crate::diagnostics::{Diagnostics, Span};
use crate::lexer::interner::InternedStr;

use super::visit::{ExprVisitor, StmtVisitor};
use super::*;

pub struct NameResolver {
    switch_stack: usize,
    used_labels: FxHashMap<InternedStr, Span>,
    labels: FxHashMap<InternedStr, Span>,
    diag: Rc<Diagnostics>,
}

impl NameResolver {
    pub fn new(diag: Rc<Diagnostics>) -> Self {
        Self {
            switch_stack: 0,
            used_labels: FxHashMap::default(),
            labels: FxHashMap::default(),
            diag,
        }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.labels.clear();
        assert_eq!(self.switch_stack, 0);
        match &def.kind {
            DefKind::Vector { .. } => {}
            DefKind::Function { body, .. } => {
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

impl StmtVisitor for NameResolver {
    fn visit_auto(&mut self, _auto: &AutoStmt) {}

    fn visit_extrn(&mut self, _extrn: &ExtrnStmt) {}

    fn visit_goto(&mut self, goto: &GotoStmt) {
        self.use_label(&goto.label);
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        self.declare_label(&label.name);
        self.visit_stmt(&label.stmt);
    }

    fn visit_case(&mut self, case: &CaseStmt, span: Span) {
        if self.switch_stack == 0 {
            self.diag
                .error(span, "case statement outside of switch.")
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

    fn visit_name(&mut self, _name: &Name) {}

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
