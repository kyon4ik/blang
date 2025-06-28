use std::collections::HashMap;

use crate::ast::visit::{ExprVisitor, StmtVisitor};
use crate::ast::{
    AssignOp, AutoDecl, BinOp, Const, ConstKind, DefAst, ExprAst, Name, Node, UnOp, UnOpKind,
};
use crate::lexer::interner::InternedStr;
use crate::lexer::token::BinOpKind;

use bstr::ByteSlice;
use cranelift::codegen::ir::{Function, StackSlot, UserFuncName};
use cranelift::codegen::{Context, verify_function};
use cranelift::module::{Linkage, Module, default_libcall_names};
use cranelift::object::{ObjectBuilder, ObjectModule, ObjectProduct};
use cranelift::prelude::types::I64;
use cranelift::prelude::{
    AbiParam, Block, Configurable, FunctionBuilder, FunctionBuilderContext, InstBuilder, IntCC,
    MemFlags, Signature, StackSlotData, StackSlotKind, Value, isa, settings,
};

pub struct CraneliftBackend {
    module: ObjectModule,
    ctx: Context,
    func_ctx: FunctionBuilderContext,
}

pub struct CraneliftFunction<'f> {
    vars: HashMap<InternedStr, StackSlot>,
    builder: FunctionBuilder<'f>,
    entry_block: Block,
}

impl CraneliftBackend {
    pub fn new(target: &str, optimize: bool) -> Self {
        let mut shared_builder = settings::builder();
        if optimize {
            shared_builder.set("opt_level", "speed").unwrap();
        }
        let shared_flags = settings::Flags::new(shared_builder);
        let isa = isa::lookup_by_name(target)
            .unwrap()
            .finish(shared_flags)
            .unwrap();

        let builder = ObjectBuilder::new(isa, "program", default_libcall_names()).unwrap();
        let module = ObjectModule::new(builder);
        let ctx = module.make_context();
        let func_ctx = FunctionBuilderContext::new();

        Self {
            module,
            func_ctx,
            ctx,
        }
    }

    pub fn visit_def(&mut self, def: &DefAst) {
        match &def.kind {
            crate::ast::DefKind::Vector { .. } => todo!(),
            crate::ast::DefKind::Function { params, body } => {
                let mut sig = self.module.make_signature();
                for _ in params {
                    sig.params.push(AbiParam::new(I64));
                }

                let func_name = def.name.value.display().to_str().unwrap();
                let func_id = self
                    .module
                    .declare_function(func_name, Linkage::Export, &sig)
                    .unwrap();

                self.ctx.func = Function::with_name_signature(UserFuncName::user(0, 0), sig);
                {
                    let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.func_ctx);

                    let entry_block = builder.create_block();
                    let mut cfunc = CraneliftFunction {
                        vars: HashMap::new(),
                        builder,
                        entry_block,
                    };

                    cfunc
                        .builder
                        .append_block_params_for_function_params(entry_block);
                    cfunc.builder.switch_to_block(entry_block);
                    for (i, param) in params.iter().enumerate() {
                        cfunc.alloc_var(param);
                        cfunc.init_var(param, cfunc.builder.block_params(cfunc.entry_block)[i]);
                    }

                    cfunc.visit_stmt(body);
                    cfunc.builder.seal_all_blocks();
                    cfunc.builder.finalize();
                }

                verify_function(&self.ctx.func, self.module.isa().flags()).unwrap();
                self.module.define_function(func_id, &mut self.ctx).unwrap();
                println!("{}", self.ctx.func.display());
            }
        }
    }

    pub fn finish(self) -> ObjectProduct {
        self.module.finish()
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum BVal {
    Lv(Value),
    Rv(Value),
}

impl BVal {
    fn to_value(self) -> Value {
        match self {
            Self::Lv(value) | Self::Rv(value) => value,
        }
    }
}

impl CraneliftFunction<'_> {
    fn alloc_var(&mut self, name: &Name) {
        let slot = self.builder.create_sized_stack_slot(StackSlotData::new(
            StackSlotKind::ExplicitSlot,
            I64.bytes(),
            3,
        ));
        self.vars.insert(name.value, slot);
    }

    fn init_var(&mut self, name: &Name, value: Value) {
        let slot = self.vars.get(&name.value).unwrap();
        self.builder.ins().stack_store(value, *slot, 0);
    }

    fn load(&mut self, value: BVal) -> Value {
        match value {
            BVal::Lv(addr) => self.builder.ins().load(I64, MemFlags::new(), addr, 0),
            BVal::Rv(value) => value,
        }
    }

    fn store(&mut self, val: BVal, p: BVal) {
        match p {
            BVal::Lv(addr) => {
                let x = self.load(val);
                self.builder.ins().store(MemFlags::new(), x, addr, 0);
            }
            _ => panic!("Store is not possible to rvalue."),
        }
    }
}

impl StmtVisitor for CraneliftFunction<'_> {
    fn visit_auto(&mut self, decls: &[AutoDecl]) {
        for decl in decls {
            self.alloc_var(&decl.name);
        }
    }

    fn visit_extrn(&mut self, _names: &[Name]) {}

    fn visit_goto(&mut self, _label: &Name) {}

    fn visit_return(&mut self, expr: Option<&ExprAst>) {
        if expr.is_none() {
            self.builder.ins().return_(&[]);
        }
    }
}

impl ExprVisitor for CraneliftFunction<'_> {
    type Value = BVal;

    fn visit_name(&mut self, name: &Name) -> Self::Value {
        let slot = self.vars.get(&name.value).unwrap();
        BVal::Lv(self.builder.ins().stack_addr(I64, *slot, 0))
    }

    fn visit_const(&mut self, cnst: &Const) -> Self::Value {
        match cnst.kind {
            ConstKind::Number(num) => {
                // FIXME: store in hashmap
                let value = num.display().to_str().unwrap().parse::<i64>().unwrap();
                BVal::Rv(self.builder.ins().iconst(I64, value))
            }
            ConstKind::Char(_) => todo!(),
            ConstKind::String(_) => todo!(),
        }
    }

    fn visit_group(&mut self, group: &ExprAst) -> Self::Value {
        let group = self.visit_expr(group);
        BVal::Rv(self.load(group))
    }

    fn visit_assign(&mut self, op: AssignOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value {
        let lhs = self.visit_expr(lhs);
        let rhs = self.visit_expr(rhs);
        if let Some(_bin_op) = op.kind {
            todo!()
        } else {
            self.store(rhs, lhs);
            lhs
        }
    }

    fn visit_unary(&mut self, op: UnOp, expr: &ExprAst) -> Self::Value {
        let expr = self.visit_expr(expr);
        match op.kind {
            UnOpKind::Neg => {
                let expr = self.load(expr);
                BVal::Rv(self.builder.ins().ineg(expr))
            }
            UnOpKind::Not => {
                let expr = self.load(expr);
                BVal::Rv(self.builder.ins().bnot(expr))
            }
            UnOpKind::Deref => BVal::Lv(expr.to_value()),
            UnOpKind::Ref => match expr {
                BVal::Lv(addr) => BVal::Rv(addr),
                _ => panic!("Can not reference rvalue."),
            },
            UnOpKind::Inc => {
                let x = self.load(expr);
                let sum = self.builder.ins().iadd_imm(x, 1);
                self.store(BVal::Rv(sum), expr);
                BVal::Rv(sum)
            }
            UnOpKind::Dec => {
                let x = self.load(expr);
                let sum = self.builder.ins().iadd_imm(x, -1);
                self.store(BVal::Rv(sum), expr);
                BVal::Rv(sum)
            }
            UnOpKind::PostInc => {
                let x = self.load(expr);
                let sum = self.builder.ins().iadd_imm(x, 1);
                self.store(BVal::Rv(sum), expr);
                BVal::Rv(x)
            }
            UnOpKind::PostDec => {
                let x = self.load(expr);
                let sum = self.builder.ins().iadd_imm(x, -1);
                self.store(BVal::Rv(sum), expr);
                BVal::Rv(x)
            }
        }
    }

    fn visit_binary(&mut self, op: BinOp, lhs: &ExprAst, rhs: &ExprAst) -> Self::Value {
        let lhs = self.visit_expr(lhs);
        let rhs = self.visit_expr(rhs);
        let lhs = self.load(lhs);
        let rhs = self.load(rhs);

        let ins = self.builder.ins();
        let res = match op.kind {
            BinOpKind::Or => ins.bor(lhs, rhs),
            BinOpKind::And => ins.band(lhs, rhs),
            BinOpKind::Eq => ins.icmp(IntCC::Equal, lhs, rhs),
            BinOpKind::Neq => ins.icmp(IntCC::NotEqual, lhs, rhs),
            BinOpKind::Lt => ins.icmp(IntCC::SignedLessThan, lhs, rhs),
            BinOpKind::LtEq => ins.icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
            BinOpKind::Gt => ins.icmp(IntCC::SignedGreaterThan, lhs, rhs),
            BinOpKind::GtEq => ins.icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
            BinOpKind::Shl => ins.ishl(lhs, rhs),
            BinOpKind::Shr => ins.ushr(lhs, rhs),
            BinOpKind::Add => ins.iadd(lhs, rhs),
            BinOpKind::Sub => ins.isub(lhs, rhs),
            BinOpKind::Rem => ins.srem(lhs, rhs),
            BinOpKind::Mul => ins.imul(lhs, rhs),
            BinOpKind::Div => ins.imul(lhs, rhs),
        };

        BVal::Rv(res)
    }

    // NOTE: addr is bytes on modern systems, but in B it is words
    fn visit_offset(
        &mut self,
        _base: &crate::ast::ExprAst,
        _offset: &crate::ast::ExprAst,
    ) -> Self::Value {
        todo!()
    }

    fn visit_ternary(
        &mut self,
        cond: &ExprAst,
        then_expr: &ExprAst,
        else_expr: &ExprAst,
    ) -> Self::Value {
        let cond = self.visit_expr(cond);
        let then_expr = self.visit_expr(then_expr);
        let else_expr = self.visit_expr(else_expr);
        let cond = self.load(cond);
        let then_expr = self.load(then_expr);
        let else_expr = self.load(else_expr);
        BVal::Rv(self.builder.ins().select(cond, then_expr, else_expr))
    }

    fn visit_call(&mut self, callee: &ExprAst, args: &[Node<ExprAst>]) -> Self::Value {
        let callee = self.visit_expr(callee);
        let callee = self.load(callee);

        let mut sig = Signature::new(isa::CallConv::SystemV);
        let mut arg_values = Vec::with_capacity(args.len());
        for arg in args {
            sig.params.push(AbiParam::new(I64));
            let val = self.visit_expr(arg);
            arg_values.push(self.load(val));
        }
        let sig_ref = self.builder.import_signature(sig);
        let inst = self
            .builder
            .ins()
            .call_indirect(sig_ref, callee, &arg_values);
        BVal::Rv(self.builder.inst_results(inst)[0])
    }
}
