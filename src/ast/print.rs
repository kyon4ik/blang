use bstr::{BStr, BString, ByteVec};

use crate::ast::DefKind;
use crate::lexer::token::LiteralKind;

use super::visit::{ExprVisitor, StmtVisitor};
use super::*;

pub struct PrettyPrinter {
    output: BString,
}

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: BString::new(vec![]),
        }
    }

    pub fn display(&self) -> &BStr {
        BStr::new(&self.output)
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.output.push_str(def.name.as_str());
        match &def.kind {
            DefKind::Vector { size, .. } => {
                self.output.push_str(" <vector");
                match size {
                    VecSize::Undef => {}
                    VecSize::Empty(_) => {
                        self.output.push_str("[]");
                    }
                    VecSize::Def { lit, .. } => {
                        self.output.push(b'[');
                        self.visit_const(lit);
                        self.output.push(b']');
                    }
                }
                self.output.push_str(">");
            }
            DefKind::Function { params, body } => {
                let params = &params.params;
                self.output.push_str(format!(" <func({})>\n", params.len()));
                for param in params {
                    self.output.push_str(param.as_str());
                    self.output.push(b' ');
                }
                if !params.is_empty() {
                    self.output.push(b'\n');
                }
                self.visit_stmt(body);
            }
        }
        self.output.push(b'\n');
    }
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl StmtVisitor for PrettyPrinter {
    fn visit_auto(&mut self, auto: &AutoStmt) {
        self.output.push_str("(auto ");
        for decl in &auto.decls {
            self.output.push(b'(');
            self.visit_name(&decl.name);
            decl.value.inspect(|v| self.visit_const(v));
            self.output.push(b')');
        }
    }

    fn visit_extrn(&mut self, extrn: &ExtrnStmt) {
        self.output.push_str("(extrn ");
        for name in &extrn.names {
            self.visit_name(name);
        }
    }

    fn visit_semi(&mut self, semi: &SemiStmt) {
        if let Some(expr) = &semi.expr {
            self.visit_expr(expr);
        } else {
            self.output.push_str(b"<empty>");
        }
    }

    fn visit_return(&mut self, return_: &ReturnStmt) {
        self.output.push_str(b"ret ");
        if let Some(expr) = &return_.expr {
            self.visit_expr(expr);
        } else {
            self.output.push_str(b"<empty>");
        }
    }

    fn visit_goto(&mut self, goto: &GotoStmt) {
        self.output.push_str(b"goto ");
        self.visit_name(&goto.label);
    }

    fn visit_cond(&mut self, cond: &CondStmt) {
        self.output.push_str("(if");
        self.visit_expr(&cond.cond);
        self.output.push(b'\t');
        self.visit_stmt(&cond.then_stmt);
        if let Some(es) = &cond.else_stmt {
            self.output.push_str(" else");
            self.output.push(b'\t');
            self.visit_stmt(es);
        }
    }

    fn visit_while(&mut self, while_: &WhileStmt) {
        self.output.push_str("(while");
        self.visit_expr(&while_.cond);
        self.output.push(b'\t');
        self.visit_stmt(&while_.stmt);
        self.output.push(b')');
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        self.output.push_str("(label ");
        self.visit_name(&label.name);
        self.visit_stmt(&label.stmt);
        self.output.push(b')');
    }

    fn visit_case(&mut self, case: &CaseStmt, _span: Span) {
        self.output.push_str("(case ");
        self.visit_const(&case.cnst);
        self.visit_stmt(&case.stmt);
        self.output.push(b')');
    }

    fn visit_switch(&mut self, switch: &SwitchStmt) {
        self.output.push_str("(switch");
        self.visit_expr(&switch.cond);
        self.output.push(b'\t');
        self.visit_stmt(&switch.stmt);
        self.output.push(b')');
    }

    fn visit_block(&mut self, block: &BlockStmt) {
        for stmt in &block.stmts {
            self.output.push_str("  ");
            self.visit_stmt(stmt);
            self.output.push(b'\n');
        }
    }
}

impl ExprVisitor for PrettyPrinter {
    type Value = ();

    fn visit_name(&mut self, name: &Name) {
        self.output.push_str(name.as_str());
    }

    fn visit_const(&mut self, cnst: &Literal) {
        let delim = match cnst.kind {
            LiteralKind::Number => None,
            LiteralKind::Char => Some(b'\''),
            LiteralKind::String => Some(b'"'),
        };
        delim.inspect(|d| self.output.push(*d));
        self.output.push_str(cnst.value.display());
        delim.inspect(|d| self.output.push(*d));
    }

    fn visit_group(&mut self, group: &GroupExpr) {
        self.visit_expr(&group.expr);
    }

    fn visit_assign(&mut self, assign: &AssignExpr) {
        let op_str: Option<&str> = assign.op.kind.map(|kind| kind.into());
        self.output.push(b'(');
        self.output.push(b'=');
        op_str.inspect(|op| self.output.push_str(op));
        self.output.push(b' ');
        self.visit_expr(&assign.lhs);
        self.output.push(b' ');
        self.visit_expr(&assign.rhs);
        self.output.push(b')');
    }

    fn visit_unary(&mut self, unary: &UnaryExpr) {
        let op_str: &str = unary.op.kind.into();
        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(&unary.expr);
        self.output.push(b')');
    }

    fn visit_binary(&mut self, binary: &BinaryExpr) {
        let op_str: &str = binary.op.kind.into();
        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(&binary.lhs);
        self.output.push(b' ');
        self.visit_expr(&binary.rhs);
        self.output.push(b')');
    }

    fn visit_offset(&mut self, offset: &OffsetExpr) {
        self.output.push_str(b"([] ");
        self.visit_expr(&offset.base);
        self.output.push(b' ');
        self.visit_expr(&offset.offset);
        self.output.push(b')');
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) {
        self.output.push_str(b"(?: ");
        self.visit_expr(&ternary.cond);
        self.output.push(b' ');
        self.visit_expr(&ternary.then_expr);
        self.output.push(b' ');
        self.visit_expr(&ternary.else_expr);
        self.output.push(b')');
    }

    fn visit_call(&mut self, call: &CallExpr) {
        self.output.push_str(b"($call ");
        self.visit_expr(&call.callee);
        for arg in &call.args {
            self.output.push(b' ');
            self.visit_expr(arg);
        }
        self.output.push(b')');
    }
}
