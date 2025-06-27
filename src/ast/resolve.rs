use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::diagnostics::{DiagErrorKind, Diagnostics, Span};
use crate::lexer::interner::InternedStr;

use super::visit::{ExprVisitor, StmtVisitor};
use super::{AutoDecl, DefAst, DefKind, Name, StmtAst};

pub struct NameResolver {
    names: HashMap<InternedStr, Span>,
    labels: HashMap<InternedStr, Span>,
    diag: Rc<RefCell<Diagnostics>>,
}

impl NameResolver {
    pub fn new(diag: Rc<RefCell<Diagnostics>>) -> Self {
        Self {
            names: HashMap::new(),
            labels: HashMap::new(),
            diag,
        }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.names.clear();
        self.labels.clear();
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

    fn visit_semi(&mut self, expr: Option<&super::ExprAst>) {
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_return(&mut self, expr: Option<&super::ExprAst>) {
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_goto(&mut self, label: &super::ExprAst) {
        self.visit_expr(label);
    }

    fn visit_case(&mut self, _cnst: &super::Const, stmt: &StmtAst) {
        self.visit_stmt(stmt);
    }

    fn visit_cond(
        &mut self,
        cond: &super::ExprAst,
        then_stmt: &StmtAst,
        else_stmt: Option<&StmtAst>,
    ) {
        self.visit_expr(cond);
        self.visit_stmt(then_stmt);
        if let Some(es) = else_stmt {
            self.visit_stmt(es);
        }
    }

    fn visit_while(&mut self, cond: &super::ExprAst, stmt: &StmtAst) {
        self.visit_expr(cond);
        self.visit_stmt(stmt);
    }

    fn visit_switch(&mut self, cond: &super::ExprAst, stmt: &StmtAst) {
        self.visit_expr(cond);
        self.visit_stmt(stmt);
    }
}

impl ExprVisitor for NameResolver {
    fn visit_name(&mut self, name: &Name) {
        if !self.names.contains_key(&name.value) {
            self.diag.borrow_mut().error(
                DiagErrorKind::other(format!("Undefined name {}", name.value.display())),
                name.span,
            )
        }
    }

    fn visit_const(&mut self, _cnst: &super::Const) {}
}
