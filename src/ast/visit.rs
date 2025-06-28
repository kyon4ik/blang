use super::*;

pub trait StmtVisitor: ExprVisitor {
    fn visit_auto(&mut self, decls: &[AutoDecl]);
    fn visit_extrn(&mut self, names: &[Name]);
    fn visit_goto(&mut self, label: &Name);

    fn visit_semi(&mut self, expr: Option<&ExprAst>) {
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_return(&mut self, expr: Option<&ExprAst>) {
        if let Some(expr) = expr {
            self.visit_expr(expr);
        }
    }

    fn visit_label(&mut self, _name: &Name, stmt: &StmtAst) {
        self.visit_stmt(stmt);
    }

    fn visit_case(&mut self, _cnst: &Const, stmt: &StmtAst) {
        self.visit_stmt(stmt);
    }

    fn visit_cond(&mut self, cond: &ExprAst, then_stmt: &StmtAst, else_stmt: Option<&StmtAst>) {
        self.visit_expr(cond);
        self.visit_stmt(then_stmt);
        if let Some(else_stmt) = else_stmt {
            self.visit_stmt(else_stmt);
        }
    }

    fn visit_while(&mut self, cond: &ExprAst, stmt: &StmtAst) {
        self.visit_expr(cond);
        self.visit_stmt(stmt);
    }

    fn visit_switch(&mut self, cond: &ExprAst, stmt: &StmtAst) {
        self.visit_expr(cond);
        self.visit_stmt(stmt);
    }

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
    type Value;

    fn visit_name(&mut self, name: &Name) -> Self::Value;
    fn visit_const(&mut self, cnst: &Const) -> Self::Value;
    fn visit_group(&mut self, group: &ExprAst) -> Self::Value;
    fn visit_assign(&mut self, op: AssignOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value;
    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) -> Self::Value;
    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value;
    fn visit_offset(&mut self, base: &ExprAst, offset: &ExprAst) -> Self::Value;
    fn visit_ternary(
        &mut self,
        cond: &ExprAst,
        then_expr: &ExprAst,
        else_expr: &ExprAst,
    ) -> Self::Value;
    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]) -> Self::Value;

    fn visit_expr(&mut self, expr: &ExprAst) -> Self::Value {
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
