use crate::ast::DefKind;
use crate::lexer::token::LiteralKind;

use super::visit::{ExprVisitor, StmtVisitor};
use super::*;

pub struct PrettyPrinter {
    out: String,
    indent: usize,
    indepth: usize,
}

impl PrettyPrinter {
    pub fn new(indent: usize) -> Self {
        Self {
            out: String::new(),
            indent,
            indepth: 0,
        }
    }

    pub fn display(&self) -> &str {
        &self.out
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        self.out.push_str(def.name.as_str());
        match &def.kind {
            DefKind::Vector { size, list } => {
                self.out.push_str("(vector ");
                self.visit_vecsize(size);
                self.indepth += 1;
                self.write_slice(list, Self::visit_immval);
                self.out.push(')');
                self.indepth -= 1;
            }
            DefKind::Function { params, body } => {
                self.out.push_str("(func");
                self.indepth += 1;
                self.write_slice(&params.params, Self::visit_name);
                self.visit_stmt(body);
                self.out.push(')');
                self.indepth -= 1;
            }
        }
        self.out.push('\n');
    }

    fn visit_immval(&mut self, ival: &ImmVal) {
        match ival {
            ImmVal::Const(_) => todo!(),
            ImmVal::Name(_) => todo!(),
        }
    }

    fn visit_vecsize(&mut self, size: &VecSize) {
        match size {
            VecSize::Undef => self.out.push('1'),
            VecSize::Empty(_) => self.out.push('0'),
            VecSize::Def { lit, .. } => self.visit_const(lit),
        }
    }

    fn visit_autodecl(&mut self, decl: &AutoDecl) {
        self.visit_name(&decl.name);
        decl.value.inspect(|v| {
            self.out.push('[');
            self.visit_const(v);
            self.out.push(']');
        });
    }

    fn write_slice<T>(&mut self, args: &[T], print: impl Fn(&mut Self, &T)) {
        if let Some((first, rest)) = args.split_first() {
            self.indent_line();
            print(self, first);
            for arg in rest {
                self.out.push(' ');
                print(self, arg);
            }
        }
    }

    fn indent_line(&mut self) {
        self.out.push('\n');
        for _ in 0..self.indent * self.indepth {
            self.out.push(' ');
        }
    }
}

impl StmtVisitor for PrettyPrinter {
    fn visit_auto(&mut self, auto: &AutoStmt) {
        self.out.push_str("(auto ");
        self.indepth += 1;
        self.write_slice(&auto.decls, Self::visit_autodecl);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_extrn(&mut self, extrn: &ExtrnStmt) {
        self.out.push_str("(extrn ");
        self.indepth += 1;
        self.write_slice(&extrn.names, Self::visit_name);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_semi(&mut self, semi: &SemiStmt) {
        self.out.push_str("(semi ");
        if let Some(expr) = &semi.expr {
            self.visit_expr(expr);
        } else {
            self.out.push_str("null");
        }
        self.out.push(')');
    }

    fn visit_return(&mut self, return_: &ReturnStmt) {
        self.out.push_str("(ret ");
        if let Some(expr) = &return_.expr {
            self.visit_expr(expr);
        } else {
            self.out.push_str("null");
        }
        self.out.push(')');
    }

    fn visit_goto(&mut self, goto: &GotoStmt) {
        self.out.push_str("(goto ");
        self.visit_name(&goto.label);
        self.out.push(')');
    }

    fn visit_cond(&mut self, cond: &CondStmt) {
        self.out.push_str("(if");
        self.indepth += 1;
        self.indent_line();
        self.visit_expr(&cond.cond);
        self.visit_stmt(&cond.then_stmt);
        if let Some(es) = &cond.else_stmt {
            self.out.push_str("(else");
            self.indepth += 1;
            self.visit_stmt(es);
            self.out.push(')');
            self.indepth -= 1;
        }
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_while(&mut self, while_: &WhileStmt) {
        self.out.push_str("(while");
        self.indepth += 1;
        self.indent_line();
        self.visit_expr(&while_.cond);
        self.visit_stmt(&while_.stmt);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        self.out.push_str("(label ");
        self.indepth += 1;
        self.visit_name(&label.name);
        self.visit_stmt(&label.stmt);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_case(&mut self, case: &CaseStmt, _span: Span) {
        self.out.push_str("(case ");
        self.indepth += 1;
        self.visit_const(&case.cnst);
        self.visit_stmt(&case.stmt);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_switch(&mut self, switch: &SwitchStmt) {
        self.out.push_str("(switch");
        self.indepth += 1;
        self.indent_line();
        self.visit_expr(&switch.cond);
        self.visit_stmt(&switch.stmt);
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_block(&mut self, block: &BlockStmt) {
        self.out.push_str("(compound");
        self.indepth += 1;
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        self.out.push(')');
        self.indepth -= 1;
    }

    fn visit_stmt(&mut self, stmt: &StmtAst) {
        self.indent_line();
        match &stmt.kind {
            StmtKind::Block(block_stmt) => self.visit_block(block_stmt),
            StmtKind::Cond(cond_stmt) => self.visit_cond(cond_stmt),
            StmtKind::While(while_stmt) => self.visit_while(while_stmt),
            StmtKind::Switch(switch_stmt) => self.visit_switch(switch_stmt),
            StmtKind::Case(case_stmt) => self.visit_case(case_stmt, stmt.span),
            StmtKind::Goto(goto_stmt) => self.visit_goto(goto_stmt),
            StmtKind::Return(return_stmt) => self.visit_return(return_stmt),
            StmtKind::Semi(semi_stmt) => self.visit_semi(semi_stmt),
            StmtKind::Extrn(extrn_stmt) => self.visit_extrn(extrn_stmt),
            StmtKind::Auto(auto_stmt) => self.visit_auto(auto_stmt),
            StmtKind::Label(label_stmt) => self.visit_label(label_stmt),
        }
    }
}

impl ExprVisitor for PrettyPrinter {
    type Value = ();

    fn visit_name(&mut self, name: &Name) {
        self.out.push_str(name.as_str());
    }

    fn visit_const(&mut self, cnst: &Literal) {
        let delim = match cnst.kind {
            LiteralKind::Number => None,
            LiteralKind::Char => Some('\''),
            LiteralKind::String => Some('"'),
        };
        delim.inspect(|d| self.out.push(*d));
        self.out.push_str(cnst.as_str());
        delim.inspect(|d| self.out.push(*d));
    }

    fn visit_group(&mut self, group: &GroupExpr) {
        self.visit_expr(&group.expr);
    }

    fn visit_assign(&mut self, assign: &AssignExpr) {
        let op_str: Option<&str> = assign.op.kind.map(|kind| kind.into());
        self.out.push('(');
        self.out.push('=');
        op_str.inspect(|op| self.out.push_str(op));
        self.out.push(' ');
        self.visit_expr(&assign.lhs);
        self.out.push(' ');
        self.visit_expr(&assign.rhs);
        self.out.push(')');
    }

    fn visit_unary(&mut self, unary: &UnaryExpr) {
        let op_str: &str = unary.op.kind.into();
        self.out.push('(');
        self.out.push_str(op_str);
        self.out.push(' ');
        self.visit_expr(&unary.expr);
        self.out.push(')');
    }

    fn visit_binary(&mut self, binary: &BinaryExpr) {
        let op_str: &str = binary.op.kind.into();
        self.out.push('(');
        self.out.push_str(op_str);
        self.out.push(' ');
        self.visit_expr(&binary.lhs);
        self.out.push(' ');
        self.visit_expr(&binary.rhs);
        self.out.push(')');
    }

    fn visit_offset(&mut self, offset: &OffsetExpr) {
        self.out.push_str("([] ");
        self.visit_expr(&offset.base);
        self.out.push(' ');
        self.visit_expr(&offset.offset);
        self.out.push(')');
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) {
        self.out.push_str("(?: ");
        self.visit_expr(&ternary.cond);
        self.out.push(' ');
        self.visit_expr(&ternary.then_expr);
        self.out.push(' ');
        self.visit_expr(&ternary.else_expr);
        self.out.push(')');
    }

    fn visit_call(&mut self, call: &CallExpr) {
        self.out.push_str("($call ");
        self.visit_expr(&call.callee);
        for arg in &call.args {
            self.out.push(' ');
            self.visit_expr(arg);
        }
        self.out.push(')');
    }
}
