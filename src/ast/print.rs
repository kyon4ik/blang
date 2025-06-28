use bstr::{BStr, BString, ByteVec};

use crate::ast::{DefKind, VectorSize};
use crate::lexer::token::BinOpKind;

use super::visit::{ExprVisitor, StmtVisitor};
use super::{
    AssignOp, AutoDecl, BinOp, Const, ConstKind, DefAst, ExprAst, Name, Node, StmtAst, UnOp,
    UnOpKind,
};

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
                    VectorSize::Undef => {}
                    VectorSize::Zero => {
                        self.output.push_str("[]");
                    }
                    VectorSize::Def(c) => {
                        self.output.push(b'[');
                        self.visit_const(c);
                        self.output.push(b']');
                    }
                }
                self.output.push_str(">");
            }
            DefKind::Function { params, body } => {
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
    }
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl StmtVisitor for PrettyPrinter {
    fn visit_auto(&mut self, decls: &[AutoDecl]) {
        for decl in decls {
            self.output
                .push_str(format!("auto {} = {:?}", decl.name.as_str(), decl.value));
            self.output.push(b'\n');
        }
    }

    fn visit_extrn(&mut self, names: &[Name]) {
        for name in names {
            self.output.push_str(format!("extrn {}", name.as_str()));
            self.output.push(b'\n');
        }
    }

    fn visit_semi(&mut self, expr: Option<&ExprAst>) {
        if let Some(expr) = expr {
            self.visit_expr(expr);
        } else {
            self.output.push_str(b"<empty>");
        }
        self.output.push(b'\n');
    }

    fn visit_return(&mut self, expr: Option<&ExprAst>) {
        self.output.push_str(b"ret ");
        if let Some(expr) = expr {
            self.visit_expr(expr);
        } else {
            self.output.push_str(b"<empty>");
        }
        self.output.push(b'\n');
    }

    fn visit_goto(&mut self, label: &ExprAst) {
        self.output.push_str(b"goto ");
        self.visit_expr(label);
        self.output.push(b'\n');
    }

    fn visit_cond(&mut self, cond: &ExprAst, then_stmt: &StmtAst, else_stmt: Option<&StmtAst>) {
        self.output.push_str("(if");
        self.output.push(b'\t');
        self.visit_expr(cond);
        self.output.push(b'\t');
        self.visit_stmt(then_stmt);
        if let Some(es) = else_stmt {
            self.output.push_str(" else");
            self.output.push(b'\t');
            self.visit_stmt(es);
        }
        self.output.push_str(")\n");
    }

    fn visit_while(&mut self, cond: &ExprAst, stmt: &StmtAst) {
        self.output.push_str("(while");
        self.output.push(b'\t');
        self.visit_expr(cond);
        self.output.push(b'\t');
        self.visit_stmt(stmt);
        self.output.push_str(")\n");
    }

    fn visit_label(&mut self, _name: &Name, _stmt: &StmtAst) {
        todo!()
    }

    fn visit_case(&mut self, _cnst: &Const, _stmt: &StmtAst) {
        todo!()
    }

    fn visit_switch(&mut self, _cond: &ExprAst, _stmt: &StmtAst) {
        todo!()
    }

    fn visit_block(&mut self, stmts: &[Node<StmtAst>]) {
        for stmt in stmts {
            self.output.push_str("| ");
            self.visit_stmt(stmt);
        }
    }
}

impl ExprVisitor for PrettyPrinter {
    type Value = ();

    fn visit_name(&mut self, name: &Name) {
        self.output.push_str(name.as_str());
    }

    fn visit_const(&mut self, cnst: &Const) {
        match cnst.kind {
            ConstKind::Number(num) => self.output.push_str(num.display()),
            ConstKind::Char(char) => {
                self.output.push(b'\'');
                self.output.push_str(char.display());
                self.output.push(b'\'');
            }
            ConstKind::String(str) => {
                self.output.push(b'"');
                self.output.push_str(str.display());
                self.output.push(b'"');
            }
        }
    }

    fn visit_group(&mut self, group: &ExprAst) {
        self.visit_expr(group);
    }

    fn visit_assign(&mut self, op: AssignOp, lhs: &ExprAst, rhs: &ExprAst) {
        let op_str: &[u8] = match op.kind {
            Some(BinOpKind::Or) => b"=|",
            Some(BinOpKind::And) => b"=&",
            Some(BinOpKind::Eq) => b"===",
            Some(BinOpKind::Neq) => b"=!=",
            Some(BinOpKind::Lt) => b"=<",
            Some(BinOpKind::LtEq) => b"=<=",
            Some(BinOpKind::Gt) => b"=>",
            Some(BinOpKind::GtEq) => b"=>=",
            Some(BinOpKind::Shl) => b"=<<",
            Some(BinOpKind::Shr) => b"=>>",
            Some(BinOpKind::Add) => b"=+",
            Some(BinOpKind::Sub) => b"=-",
            Some(BinOpKind::Rem) => b"=%",
            Some(BinOpKind::Mul) => b"=*",
            Some(BinOpKind::Div) => b"=/",
            None => b"=",
        };
        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(lhs);
        self.output.push(b' ');
        self.visit_expr(rhs);
        self.output.push(b')');
    }

    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) {
        let op_str: &[u8] = match op.kind {
            UnOpKind::Neg => b"-",
            UnOpKind::Not => b"!",
            UnOpKind::Inc => b"++",
            UnOpKind::Dec => b"--",
            UnOpKind::Ref => b"&",
            UnOpKind::Deref => b"*",
            UnOpKind::PostInc => b"$++",
            UnOpKind::PostDec => b"$--",
        };

        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(expr);
        self.output.push(b')');
    }

    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst) {
        let op_str: &[u8] = match op.kind {
            BinOpKind::Or => b"|",
            BinOpKind::And => b"&",
            BinOpKind::Eq => b"==",
            BinOpKind::Neq => b"!=",
            BinOpKind::Lt => b"<",
            BinOpKind::LtEq => b"<=",
            BinOpKind::Gt => b">",
            BinOpKind::GtEq => b">=",
            BinOpKind::Shl => b"<<",
            BinOpKind::Shr => b">>",
            BinOpKind::Add => b"+",
            BinOpKind::Sub => b"-",
            BinOpKind::Rem => b"%",
            BinOpKind::Mul => b"*",
            BinOpKind::Div => b"/",
        };
        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(lhs);
        self.output.push(b' ');
        self.visit_expr(rhs);
        self.output.push(b')');
    }

    fn visit_offset(&mut self, base: &ExprAst, offset: &ExprAst) {
        self.output.push_str(b"([] ");
        self.visit_expr(base);
        self.output.push(b' ');
        self.visit_expr(offset);
        self.output.push(b')');
    }

    fn visit_ternary(&mut self, cond: &ExprAst, then_expr: &ExprAst, else_expr: &ExprAst) {
        self.output.push_str(b"(?: ");
        self.visit_expr(cond);
        self.output.push(b' ');
        self.visit_expr(then_expr);
        self.output.push(b' ');
        self.visit_expr(else_expr);
        self.output.push(b')');
    }

    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]) {
        self.output.push_str(b"($call ");
        self.visit_expr(callee);
        for arg in args {
            self.output.push(b' ');
            self.visit_expr(arg);
        }
        self.output.push(b')');
    }
}
