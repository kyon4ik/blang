use super::*;

pub trait StmtVisitor: ExprVisitor {
    fn visit_auto(&mut self, auto: &AutoStmt);
    fn visit_extrn(&mut self, extrn: &ExtrnStmt);
    fn visit_goto(&mut self, label: &GotoStmt);

    fn visit_semi(&mut self, semi: &SemiStmt) {
        if let Some(expr) = &semi.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_return(&mut self, return_: &ReturnStmt) {
        if let Some(expr) = &return_.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        self.visit_stmt(&label.stmt);
    }

    fn visit_case(&mut self, case: &CaseStmt, _span: Span) {
        self.visit_stmt(&case.stmt);
    }

    fn visit_cond(&mut self, cond: &CondStmt) {
        self.visit_expr(&cond.cond);
        self.visit_stmt(&cond.then_stmt);
        if let Some(else_stmt) = &cond.else_stmt {
            self.visit_stmt(else_stmt);
        }
    }

    fn visit_while(&mut self, while_: &WhileStmt) {
        self.visit_expr(&while_.cond);
        self.visit_stmt(&while_.stmt);
    }

    fn visit_switch(&mut self, switch: &SwitchStmt) {
        self.visit_expr(&switch.cond);
        self.visit_stmt(&switch.stmt);
    }

    fn visit_block(&mut self, block: &BlockStmt) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &StmtAst) {
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

pub trait ExprVisitor {
    type Value;

    fn visit_name(&mut self, name: &Name) -> Self::Value;
    fn visit_const(&mut self, cnst: &Literal) -> Self::Value;
    fn visit_group(&mut self, group: &GroupExpr) -> Self::Value;
    fn visit_assign(&mut self, assign: &AssignExpr) -> Self::Value;
    fn visit_unary(&mut self, unary: &UnaryExpr) -> Self::Value;
    fn visit_binary(&mut self, binary: &BinaryExpr) -> Self::Value;
    fn visit_offset(&mut self, offset: &OffsetExpr) -> Self::Value;
    fn visit_ternary(&mut self, ternary: &TernaryExpr) -> Self::Value;
    fn visit_call(&mut self, call: &CallExpr) -> Self::Value;

    fn visit_expr(&mut self, expr: &ExprAst) -> Self::Value {
        match &expr.kind {
            ExprKind::Name(name) => self.visit_name(name),
            ExprKind::Const(cnst) => self.visit_const(cnst),
            ExprKind::Group(group) => self.visit_group(group),
            ExprKind::Assign(assign) => self.visit_assign(assign),
            ExprKind::Unary(unary) => self.visit_unary(unary),
            ExprKind::Binary(binary) => self.visit_binary(binary),
            ExprKind::Offset(offset) => self.visit_offset(offset),
            ExprKind::Ternary(ternary) => self.visit_ternary(ternary),
            ExprKind::Call(call) => self.visit_call(call),
        }
    }
}
