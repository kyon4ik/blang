use bstr::{BString, ByteVec};

use crate::ast::{DefKind, VectorSize};
use crate::lexer::BinOp;

use super::{AutoDecl, Const, ConstKind, DefAst, ExprAst, ImmVal, Name, Node, StmtAst, UnOp};

pub trait FunctionVisitor {
    fn visit_function(&mut self, name: &Name, params: &[Name], body: &StmtAst);
}

pub trait VectorVisitor {
    fn visit_vector(&mut self, name: &Name, values: &[ImmVal]);
}

pub trait StmtVisitor {
    fn visit_auto(&mut self, decls: &[AutoDecl]);
    fn visit_extrn(&mut self, names: &[Name]);
    fn visit_semi(&mut self, expr: Option<&ExprAst>);
    fn visit_return(&mut self, expr: Option<&ExprAst>);
    fn visit_label(&mut self, name: &Name, stmt: &StmtAst);
    fn visit_goto(&mut self, label: &ExprAst);
    fn visit_case(&mut self, cnst: &Const, stmt: &StmtAst);
    fn visit_cond(&mut self, cond: &ExprAst, then_stmt: &StmtAst, else_stmt: Option<&StmtAst>);
    fn visit_while(&mut self, cond: &ExprAst, stmt: &StmtAst);
    fn visit_switch(&mut self, cond: &ExprAst, stmt: &StmtAst);

    fn visit_block(&mut self, stmts: &[Node<StmtAst>]) {
        for stmt in stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &StmtAst) {
        match stmt {
            StmtAst::Block(nodes) => self.visit_block(nodes),
            StmtAst::Cond {
                cond,
                then_stmt,
                else_stmt,
            } => self.visit_cond(cond, then_stmt, else_stmt.as_deref()),
            StmtAst::While { cond, stmt } => self.visit_while(cond, stmt),
            StmtAst::Switch { cond, stmt } => self.visit_switch(cond, stmt),
            StmtAst::Case { cnst, stmt } => self.visit_case(cnst, stmt),
            StmtAst::Goto(node) => self.visit_goto(node),
            StmtAst::Return(node) => self.visit_return(node.as_deref()),
            StmtAst::Semi(node) => self.visit_semi(node.as_deref()),
            StmtAst::Extrn(names) => self.visit_extrn(names),
            StmtAst::Auto(auto_decls) => self.visit_auto(auto_decls),
            StmtAst::Label { name, stmt } => self.visit_label(name, stmt),
        }
    }
}

pub trait ExprVisitor {
    fn visit_name(&mut self, name: &Name);
    fn visit_const(&mut self, cnst: &Const);
    fn visit_group(&mut self, group: &ExprAst);
    fn visit_assign(&mut self, op: Option<BinOp>, lhs: &ExprAst, rhs: &ExprAst);
    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst);
    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst);
    fn visit_offset(&mut self, base: &ExprAst, offset: &ExprAst);
    fn visit_ternary(&mut self, cond: &ExprAst, then_expr: &ExprAst, else_expr: &ExprAst);
    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]);

    fn visit_expr(&mut self, expr: &ExprAst) {
        match expr {
            ExprAst::Name(name) => self.visit_name(name),
            ExprAst::Const(cnst) => self.visit_const(cnst),
            ExprAst::Group(node) => self.visit_group(node),
            ExprAst::Assign { op, lhs, rhs } => self.visit_assign(*op, lhs, rhs),
            ExprAst::Unary { op, expr } => self.visit_unary(*op, expr),
            ExprAst::Binary { op, lhs, rhs } => self.visit_binary(*op, lhs, rhs),
            ExprAst::Offset { base, offset } => self.visit_offset(base, offset),
            ExprAst::Ternary {
                cond,
                then_expr,
                else_expr,
            } => self.visit_ternary(cond, then_expr, else_expr),
            ExprAst::Call { callee, args } => self.visit_call(callee, args),
        }
    }
}

pub struct PrettyPrinter {
    output: BString,
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

impl PrettyPrinter {
    pub fn new() -> Self {
        Self {
            output: BString::new(vec![]),
        }
    }

    pub fn print(&mut self, def: &DefAst) {
        self.output.clear();
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
                        let mut sprinter = SExprVisitor::new();
                        sprinter.visit_const(c);
                        self.output.push(b'[');
                        self.output.push_str(sprinter.into_inner());
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
        println!("{}", self.output);
    }

    pub fn visit_expr(&mut self, expr: &ExprAst) {
        let mut s = SExprVisitor::new();
        s.visit_expr(expr);
        self.output.push_str(s.into_inner());
    }
}

impl Default for PrettyPrinter {
    fn default() -> Self {
        Self::new()
    }
}

pub struct SExprVisitor {
    output: BString,
}

impl SExprVisitor {
    pub fn new() -> Self {
        Self {
            output: BString::new(vec![]),
        }
    }

    pub fn into_inner(self) -> BString {
        self.output
    }
}

impl Default for SExprVisitor {
    fn default() -> Self {
        Self::new()
    }
}

impl ExprVisitor for SExprVisitor {
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

    fn visit_assign(&mut self, op: Option<BinOp>, lhs: &ExprAst, rhs: &ExprAst) {
        let op_str: &[u8] = match op {
            Some(BinOp::Or) => b"=|",
            Some(BinOp::And) => b"=&",
            Some(BinOp::Eq) => b"===",
            Some(BinOp::Neq) => b"=!=",
            Some(BinOp::Lt) => b"=<",
            Some(BinOp::LtEq) => b"=<=",
            Some(BinOp::Gt) => b"=>",
            Some(BinOp::GtEq) => b"=>=",
            Some(BinOp::Shl) => b"=<<",
            Some(BinOp::Shr) => b"=>>",
            Some(BinOp::Add) => b"=+",
            Some(BinOp::Sub) => b"=-",
            Some(BinOp::Rem) => b"=%",
            Some(BinOp::Mul) => b"=*",
            Some(BinOp::Div) => b"=/",
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
        let op_str: &[u8] = match op {
            UnOp::Neg => b"-",
            UnOp::Not => b"!",
            UnOp::Inc => b"++",
            UnOp::Dec => b"--",
            UnOp::Ref => b"&",
            UnOp::Deref => b"*",
            UnOp::PostInc => b"$++",
            UnOp::PostDec => b"$--",
        };

        self.output.push(b'(');
        self.output.push_str(op_str);
        self.output.push(b' ');
        self.visit_expr(expr);
        self.output.push(b')');
    }

    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst) {
        let op_str: &[u8] = match op {
            BinOp::Or => b"|",
            BinOp::And => b"&",
            BinOp::Eq => b"==",
            BinOp::Neq => b"!=",
            BinOp::Lt => b"<",
            BinOp::LtEq => b"<=",
            BinOp::Gt => b">",
            BinOp::GtEq => b">=",
            BinOp::Shl => b"<<",
            BinOp::Shr => b">>",
            BinOp::Add => b"+",
            BinOp::Sub => b"-",
            BinOp::Rem => b"%",
            BinOp::Mul => b"*",
            BinOp::Div => b"/",
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
