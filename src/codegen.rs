use std::path::Path;
use std::rc::Rc;

use crate::ast::visit::{ExprVisitor, StmtVisitor};
use crate::ast::*;
use crate::diagnostics::{Diagnostics, Span};
use crate::lexer::interner::InternedStr;
use crate::lexer::token::{BinOpKind, LiteralKind};

use bstr::ByteSlice;
use cranelift::codegen::ir as clir;
use cranelift::codegen::{self as clb, settings::Configurable as _};
use cranelift::frontend as clf;
use cranelift::module::{self as clm, Module as _};
use cranelift::object as clo;
use cranelift::prelude::InstBuilder;
use rustc_hash::FxHashMap;

pub struct Module {
    module: clo::ObjectModule,
    ctx: clb::Context,
    func_ctx: clf::FunctionBuilderContext,
    word_type: clir::Type,
    globals: FxHashMap<InternedStr, GlobalInfo>,
    diag: Rc<Diagnostics>,
}

struct Function<'f> {
    builder: clf::FunctionBuilder<'f>,
    labels: FxHashMap<InternedStr, clir::Block>,
    autos: FxHashMap<InternedStr, AutoInfo>,
    numbers: FxHashMap<InternedStr, i64>,
    strings: FxHashMap<InternedStr, clir::GlobalValue>,
    signatures: FxHashMap<usize, clir::SigRef>,
    word_type: clir::Type,
    return_block: clir::Block,
    globals: &'f FxHashMap<InternedStr, GlobalInfo>,
    module: &'f mut clo::ObjectModule,
    diag: &'f Diagnostics,
}

struct GlobalInfo {
    id: clm::FuncOrDataId,
    span: Span,
}

impl Module {
    pub fn new(target: &str, path: &Path, optimize: bool, diag: Rc<Diagnostics>) -> Self {
        let mut shared_builder = clb::settings::builder();
        shared_builder.enable("is_pic").unwrap();
        if optimize {
            shared_builder.set("opt_level", "speed_and_size").unwrap();
        }
        let shared_flags = clb::settings::Flags::new(shared_builder);
        let isa = clb::isa::lookup_by_name(target)
            .unwrap()
            .finish(shared_flags)
            .unwrap();

        let word_type = isa.pointer_type();
        let builder = clo::ObjectBuilder::new(
            isa,
            path.as_os_str().as_encoded_bytes(),
            clm::default_libcall_names(),
        )
        .unwrap();
        let module = clo::ObjectModule::new(builder);
        let ctx = module.make_context();
        let func_ctx = clf::FunctionBuilderContext::new();

        Self {
            module,
            func_ctx,
            ctx,
            word_type,
            globals: FxHashMap::default(),
            diag,
        }
    }

    pub fn run_global_pass(&mut self, program: &[DefAst]) {
        for def in program {
            self.run_global_pass_on(def);
        }
    }

    pub fn run_local_pass(&mut self, program: &[DefAst], print: bool) {
        for def in program {
            self.run_local_pass_on(def, print);
        }
    }

    fn run_global_pass_on(&mut self, def: &DefAst) {
        let global_name = def.name.value.display().to_str().unwrap();
        let id = match &def.kind {
            DefKind::Vector { .. } => {
                let data_id = self
                    .module
                    .declare_data(global_name, clm::Linkage::Export, true, false)
                    .unwrap();
                clm::FuncOrDataId::Data(data_id)
            }
            DefKind::Function { params, .. } => {
                let params = &params.params;
                let mut sig = self.module.make_signature();
                sig.params.extend(std::iter::repeat_n(
                    clir::AbiParam::new(self.word_type),
                    params.len(),
                ));
                sig.returns.push(clir::AbiParam::new(self.word_type));

                let func_id = self
                    .module
                    .declare_function(global_name, clm::Linkage::Export, &sig)
                    .unwrap();
                clm::FuncOrDataId::Func(func_id)
            }
        };
        self.globals
            .entry(def.name.value)
            .and_modify(|global| {
                self.diag
                    .error(
                        def.name.span,
                        format!("redefinition of global name {}", def.name.value.display()),
                    )
                    .add_label(global.span, "first defined here")
                    .finish();
            })
            .or_insert(GlobalInfo {
                id,
                span: def.name.span,
            });
    }

    fn run_local_pass_on(&mut self, def: &DefAst, print: bool) {
        let global = self
            .globals
            .get(&def.name.value)
            .expect("Run local pass to define globals");
        match &def.kind {
            DefKind::Vector { .. } => {
                todo!("Vector is not implemented")
                // let data_id = match global.id {
                //     clm::FuncOrDataId::Data(data_id) => data_id,
                //     _ => panic!("Expected data id in globals table"),
                // };
                // let decl_size = match size {
                //     VecSize::Undef => 1,
                //     VecSize::Empty(_) => 0,
                //     VecSize::Def { lit, .. } => {
                //         let bytes = lit.value.display();
                //         debug_assert!(bytes.is_ascii());
                //         match lit.kind {
                //             LiteralKind::Number => {
                //                 // SAFETY: number only consists of digits, it maybe not true for other literals
                //                 let str = unsafe { bytes.to_str_unchecked() };
                //                 str.parse::<usize>().unwrap()
                //             }
                //             LiteralKind::Char => todo!(),
                //             LiteralKind::String => todo!(),
                //         }
                //     }
                // };
                // let desc = clm::DataDescription {
                //     init: clm::Init::Zeros {
                //         size: decl_size.max(list.len()) * self.word_type.bytes() as usize,
                //     },
                //     function_decls: todo!(),
                //     data_decls: todo!(),
                //     function_relocs: todo!(),
                //     data_relocs: todo!(),
                //     custom_segment_section: todo!(),
                //     align: todo!(),
                // };
                // self.module.define_data(data_id, &desc).unwrap();
            }
            DefKind::Function { params, body } => {
                let func_id = match global.id {
                    clm::FuncOrDataId::Func(func_id) => func_id,
                    _ => panic!("Expected function id in globals table"),
                };
                self.ctx.clear();
                self.ctx.func.name =
                    clir::UserFuncName::User(clir::UserExternalName::new(0, func_id.as_u32()));
                self.ctx.func.signature = self
                    .module
                    .declarations()
                    .get_function_decl(func_id)
                    .signature
                    .clone();

                let cfunc = Function::new(self);
                cfunc.build(params, body);

                if self.diag.has_errors() {
                    self.func_ctx = clf::FunctionBuilderContext::new();
                    return;
                }

                clb::verify_function(&self.ctx.func, self.module.isa().flags()).unwrap();
                if print {
                    println!("{}", self.ctx.func.display());
                }

                self.module.define_function(func_id, &mut self.ctx).unwrap();
            }
        }
    }

    pub fn finish(self) -> clo::ObjectProduct {
        self.module.finish()
    }
}

impl<'f> Function<'f> {
    fn new(module: &'f mut Module) -> Self {
        let builder = clf::FunctionBuilder::new(&mut module.ctx.func, &mut module.func_ctx);

        Self {
            builder,
            labels: FxHashMap::default(),
            autos: FxHashMap::default(),
            numbers: FxHashMap::default(),
            strings: FxHashMap::default(),
            signatures: FxHashMap::default(),
            word_type: module.word_type,
            return_block: clir::Block::from_u32(0),
            globals: &module.globals,
            module: &mut module.module,
            diag: &module.diag,
        }
    }

    fn build(mut self, params: &FuncParams, body: &StmtAst) {
        // entry block
        let entry_block = self.builder.create_block();
        self.builder
            .append_block_params_for_function_params(entry_block);
        self.builder.switch_to_block(entry_block);
        let entry_block_params: Vec<_> = self.builder.block_params(entry_block).to_vec();
        for (name, value) in params.params.iter().zip(entry_block_params) {
            self.create_param(name, value);
        }

        // exit block
        self.return_block = self.builder.create_block();

        self.visit_stmt(body);

        if self.diag.has_errors() {
            return;
        }

        if let Some(block) = self.builder.current_block() {
            if self.builder.func.block_successors(block).next().is_none() {
                let zero = self.builder.ins().iconst(self.word_type, 0);
                self.builder
                    .ins()
                    .jump(self.return_block, &[clir::BlockArg::Value(zero)]);
            }
        }

        // fill exit block
        self.builder
            .append_block_params_for_function_returns(self.return_block);
        self.builder.switch_to_block(self.return_block);
        let returns = self.builder.block_params(self.return_block).to_vec();
        self.builder.ins().return_(&returns);

        self.builder.seal_all_blocks();
        self.builder.finalize();
    }

    fn declare_auto(&mut self, name: &Name, value: clir::Value, is_global: bool) {
        self.autos
            .entry(name.value)
            .and_modify(|info| {
                self.diag
                    .error(
                        name.span,
                        format!("redefinition of name {}", name.value.display()),
                    )
                    .add_label(info.span, "first defined here")
                    .finish();
            })
            .or_insert(AutoInfo {
                value,
                is_global,
                span: name.span,
            });
    }

    // TODO: alloc with constant
    fn create_auto(&mut self, name: &Name) {
        let slot = self
            .builder
            .create_sized_stack_slot(clir::StackSlotData::new(
                clir::StackSlotKind::ExplicitSlot,
                self.word_type.bytes(),
                self.word_type.bytes().ilog2() as u8,
            ));

        let addr = self.builder.ins().stack_addr(self.word_type, slot, 0);
        self.declare_auto(name, addr, false);
    }

    fn create_param(&mut self, name: &Name, value: clir::Value) {
        let slot = self
            .builder
            .create_sized_stack_slot(clir::StackSlotData::new(
                clir::StackSlotKind::ExplicitSlot,
                self.word_type.bytes(),
                self.word_type.bytes().ilog2() as u8,
            ));

        self.builder.ins().stack_store(value, slot, 0);

        let addr = self.builder.ins().stack_addr(self.word_type, slot, 0);
        self.declare_auto(name, addr, false);
    }

    fn create_literal(&mut self, literal: &Literal) -> clir::Value {
        if let Some(val) = self.numbers.get(&literal.value) {
            self.builder.ins().iconst(self.word_type, *val)
        } else {
            // FIXME: implement custom parsing
            let bytes = literal.value.display();
            debug_assert!(bytes.is_ascii());
            match literal.kind {
                LiteralKind::Number => {
                    // SAFETY: number only consists of digits, it maybe not true for other literals
                    let str = unsafe { bytes.to_str_unchecked() };
                    let num = str.parse::<i64>().unwrap();
                    self.builder.ins().iconst(self.word_type, num)
                }
                LiteralKind::Char => {
                    let char = match bytes.len() {
                        0 => {
                            self.diag
                                .error(literal.span, "empty character is not supported")
                                .finish();
                            None
                        }
                        1 => Some(bytes[0] as u16),
                        2 => {
                            if bytes[0] != b'*' {
                                Some((bytes[0] as u16) << u8::BITS | bytes[1] as u16)
                            } else {
                                let c = unescape_char(bytes[1]);
                                if c.is_none() {
                                    self.diag
                                        .error(literal.span, "unknown escape symbol")
                                        .finish();
                                }
                                c.map(u16::from)
                            }
                        }
                        _ => {
                            self.diag
                                .error(literal.span, "character constant is longer than 2 symbols")
                                .finish();
                            None
                        }
                    };
                    if let Some(c) = char {
                        self.builder.ins().iconst(self.word_type, c as i64)
                    } else {
                        clir::Value::from_u32(0)
                    }
                }
                LiteralKind::String => {
                    let gv = self.strings.entry(literal.value).or_insert_with(|| {
                        let data_id = self.module.declare_anonymous_data(true, false).unwrap();
                        let mut desc = clm::DataDescription::new();
                        let mut data = match unescape(literal.value.display()) {
                            Ok(data) => data,
                            Err(place) => {
                                let start = literal.span.start + place as u32;
                                self.diag
                                    .error(Span::new(start, start + 1), "unknown escape symbol")
                                    .finish();
                                return clir::GlobalValue::from_u32(0);
                            }
                        };
                        data.push(b'\0');
                        desc.define(data.into_boxed_slice());
                        self.module.define_data(data_id, &desc).unwrap();
                        self.module.declare_data_in_func(data_id, self.builder.func)
                    });
                    self.builder.ins().global_value(self.word_type, *gv)
                }
            }
        }
    }

    fn make_rvalue(&mut self, value: &mut Value) {
        if value.is_lvalue {
            value.inner =
                self.builder
                    .ins()
                    .load(self.word_type, clir::MemFlags::trusted(), value.inner, 0);
            value.is_lvalue = false;
        }
    }

    fn try_store(&mut self, mut value: Value, addr: Value) -> Option<()> {
        if !addr.is_lvalue {
            return None;
        }
        self.make_rvalue(&mut value);
        self.builder
            .ins()
            .store(clir::MemFlags::trusted(), value.inner, addr.inner, 0);
        Some(())
    }

    fn incdec(&mut self, x: Value, imm: i64, is_post: bool) -> Option<clir::Value> {
        if !x.is_lvalue {
            return None;
        }

        let a = self
            .builder
            .ins()
            .load(self.word_type, clir::MemFlags::trusted(), x.inner, 0);
        let b = self.builder.ins().iadd_imm(a, imm);
        self.builder
            .ins()
            .store(clir::MemFlags::trusted(), b, x.inner, 0);
        Some(if is_post { a } else { b })
    }

    fn expected_lvalue(&self, unary: &UnaryExpr) {
        self.diag
            .error(
                unary.op.span,
                format!("'{}' expects lvalue as operand", unary.op.kind),
            )
            .add_label(unary.expr.span, "found rvalue")
            .finish();
    }
}

#[inline]
fn unescape_char(char: u8) -> Option<u8> {
    match char {
        b'0' | b'e' => Some(b'\0'),
        b'(' => Some(b'{'),
        b')' => Some(b'}'),
        b't' => Some(b'\t'),
        b'*' => Some(b'*'),
        b'\'' => Some(b'\''),
        b'"' => Some(b'"'),
        b'n' => Some(b'\n'),
        _ => None,
    }
}

fn unescape(str: &[u8]) -> Result<Vec<u8>, usize> {
    let mut res = Vec::new();
    let mut iter = str.iter().copied().enumerate();
    while let Some((_, c)) = iter.next() {
        if c == b'*'
            && let Some((i2, c2)) = iter.next()
        {
            res.push(unescape_char(c2).ok_or(i2)?);
        } else {
            res.push(c);
        }
    }
    Ok(res)
}

impl StmtVisitor for Function<'_> {
    fn visit_auto(&mut self, auto: &AutoStmt) {
        for decl in &auto.decls {
            // TODO: use const
            self.create_auto(&decl.name);
        }
    }

    fn visit_extrn(&mut self, extrn: &ExtrnStmt) {
        for name in &extrn.names {
            if let Some(global) = self.globals.get(&name.value) {
                match global.id {
                    clm::FuncOrDataId::Func(func_id) => {
                        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                        let addr = self.builder.ins().func_addr(self.word_type, func_ref);
                        self.declare_auto(name, addr, true);
                    }
                    clm::FuncOrDataId::Data(data_id) => {
                        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
                        let value = self.builder.ins().global_value(self.word_type, gv);
                        self.declare_auto(name, value, true);
                    }
                }
            } else {
                let global_name = name.value.display().to_str().unwrap();
                let mut signature = self.module.make_signature();
                signature.params.push(clir::AbiParam::new(self.word_type));
                signature.returns.push(clir::AbiParam::new(self.word_type));
                let func_id = self
                    .module
                    .declare_function(global_name, clm::Linkage::Import, &signature)
                    .unwrap();
                let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
                let addr = self.builder.ins().func_addr(self.word_type, func_ref);
                self.declare_auto(name, addr, true);
            }
        }
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        let label_block = self.builder.create_block();
        self.builder.ins().jump(label_block, &[]);
        self.builder.switch_to_block(label_block);
        self.labels.insert(label.name.value, label_block);
        self.visit_stmt(&label.stmt);
    }

    fn visit_goto(&mut self, goto: &GotoStmt) {
        if let Some(label_block) = self.labels.get(&goto.label.value) {
            self.builder.ins().jump(*label_block, &[]);
            let after_goto = self.builder.create_block();
            self.builder.switch_to_block(after_goto);
        } else {
            todo!()
        }
    }

    fn visit_return(&mut self, return_: &ReturnStmt) {
        let val = match &return_.expr {
            Some(expr) => {
                let mut res = self
                    .visit_expr(expr)
                    .unwrap_or(Value::rvalue(clir::Value::from_u32(0)));
                self.make_rvalue(&mut res);
                res.inner
            }
            None => self.builder.ins().iconst(self.word_type, 0),
        };
        self.builder
            .ins()
            .jump(self.return_block, &[clir::BlockArg::Value(val)]);
    }
}

impl ExprVisitor for Function<'_> {
    type Value = Option<Value>;

    fn visit_name(&mut self, name: &Name) -> Self::Value {
        if let Some(info) = self.autos.get(&name.value) {
            Some(if info.is_global {
                Value::rvalue(info.value)
            } else {
                Value::lvalue(info.value)
            })
        } else {
            self.diag
                .error(
                    name.span,
                    format!("undefined name {}", name.value.display()),
                )
                .finish();
            None
        }
    }

    fn visit_const(&mut self, cnst: &Literal) -> Self::Value {
        Some(Value::rvalue(self.create_literal(cnst)))
    }

    fn visit_group(&mut self, group: &GroupExpr) -> Self::Value {
        let mut res = self.visit_expr(&group.expr)?;
        self.make_rvalue(&mut res);
        Some(res)
    }

    fn visit_assign(&mut self, assign: &AssignExpr) -> Self::Value {
        let mut lhs = self.visit_expr(&assign.lhs)?;
        let rhs = self.visit_expr(&assign.rhs)?;
        if let Some(_bin_op) = assign.op.kind {
            todo!()
        } else if self.try_store(rhs, lhs).is_some() {
            self.make_rvalue(&mut lhs);
            Some(lhs)
        } else {
            self.diag
                .error(assign.op.span, "left operand of assignment must be lvalue")
                .add_label(assign.lhs.span, "found rvalue")
                .finish();
            None
        }
    }

    fn visit_unary(&mut self, unary: &UnaryExpr) -> Self::Value {
        let mut expr = self.visit_expr(&unary.expr)?;

        use UnOpKind::*;
        match unary.op.kind {
            Neg => {
                self.make_rvalue(&mut expr);
                Some(Value::rvalue(self.builder.ins().ineg(expr.inner)))
            }
            Not => {
                self.make_rvalue(&mut expr);
                Some(Value::rvalue(self.builder.ins().bnot(expr.inner)))
            }
            Deref => Some(Value::lvalue(expr.inner)),
            Ref => {
                if expr.is_lvalue {
                    Some(Value::rvalue(expr.inner))
                } else {
                    self.expected_lvalue(unary);
                    None
                }
            }
            Inc => match self.incdec(expr, 1, false) {
                Some(val) => Some(Value::rvalue(val)),
                None => {
                    self.expected_lvalue(unary);
                    None
                }
            },
            PostInc => match self.incdec(expr, 1, true) {
                Some(val) => Some(Value::rvalue(val)),
                None => {
                    self.expected_lvalue(unary);
                    None
                }
            },
            Dec => match self.incdec(expr, -1, false) {
                Some(val) => Some(Value::rvalue(val)),
                None => {
                    self.expected_lvalue(unary);
                    None
                }
            },
            PostDec => match self.incdec(expr, -1, true) {
                Some(val) => Some(Value::rvalue(val)),
                None => {
                    self.expected_lvalue(unary);
                    None
                }
            },
        }
    }

    fn visit_binary(&mut self, binary: &BinaryExpr) -> Self::Value {
        let mut lhs = self.visit_expr(&binary.lhs)?;
        let mut rhs = self.visit_expr(&binary.rhs)?;
        self.make_rvalue(&mut lhs);
        self.make_rvalue(&mut rhs);
        let lhs = lhs.inner;
        let rhs = rhs.inner;

        let ins = self.builder.ins();
        let res = match binary.op.kind {
            BinOpKind::Or => ins.bor(lhs, rhs),
            BinOpKind::And => ins.band(lhs, rhs),
            BinOpKind::Eq => ins.icmp(ICmp::Eq, lhs, rhs),
            BinOpKind::Neq => ins.icmp(ICmp::Neq, lhs, rhs),
            BinOpKind::Lt => ins.icmp(ICmp::Slt, lhs, rhs),
            BinOpKind::LtEq => ins.icmp(ICmp::Slte, lhs, rhs),
            BinOpKind::Gt => ins.icmp(ICmp::Sgt, lhs, rhs),
            BinOpKind::GtEq => ins.icmp(ICmp::Sgte, lhs, rhs),
            BinOpKind::Shl => ins.ishl(lhs, rhs),
            BinOpKind::Shr => ins.ushr(lhs, rhs),
            BinOpKind::Add => ins.iadd(lhs, rhs),
            BinOpKind::Sub => ins.isub(lhs, rhs),
            BinOpKind::Rem => ins.srem(lhs, rhs),
            BinOpKind::Mul => ins.imul(lhs, rhs),
            BinOpKind::Div => ins.imul(lhs, rhs),
        };

        Some(Value::rvalue(res))
    }

    // NOTE: addr is bytes on modern systems, but in B it is words
    fn visit_offset(&mut self, _offset: &OffsetExpr) -> Self::Value {
        todo!()
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) -> Self::Value {
        let mut cond = self.visit_expr(&ternary.cond)?;
        let mut then_expr = self.visit_expr(&ternary.then_expr)?;
        let mut else_expr = self.visit_expr(&ternary.else_expr)?;
        self.make_rvalue(&mut cond);
        self.make_rvalue(&mut then_expr);
        self.make_rvalue(&mut else_expr);
        let res = self
            .builder
            .ins()
            .select(cond.inner, then_expr.inner, else_expr.inner);
        Some(Value::rvalue(res))
    }

    fn visit_call(&mut self, call: &CallExpr) -> Self::Value {
        let mut callee = self.visit_expr(&call.callee)?;
        self.make_rvalue(&mut callee);

        let mut arg_values = Vec::with_capacity(call.args.len());
        for arg in &call.args {
            let mut val = self.visit_expr(arg)?;
            self.make_rvalue(&mut val);
            arg_values.push(val.inner);
        }
        let sig_ref = self.signatures.entry(call.args.len()).or_insert_with(|| {
            let mut sig = self.module.make_signature();
            sig.params.extend(std::iter::repeat_n(
                clir::AbiParam::new(self.word_type),
                call.args.len(),
            ));
            sig.returns.push(clir::AbiParam::new(self.word_type));
            self.builder.import_signature(sig)
        });
        let inst = self
            .builder
            .ins()
            .call_indirect(*sig_ref, callee.inner, &arg_values);
        Some(Value::rvalue(self.builder.inst_results(inst)[0]))
    }
}

#[derive(Clone, Copy, Debug)]
struct Value {
    inner: clir::Value,
    is_lvalue: bool,
}

impl Value {
    fn rvalue(inner: clir::Value) -> Self {
        Self {
            inner,
            is_lvalue: false,
        }
    }

    fn lvalue(inner: clir::Value) -> Self {
        Self {
            inner,
            is_lvalue: true,
        }
    }
}

#[derive(Clone, Copy)]
struct AutoInfo {
    value: clir::Value,
    is_global: bool,
    span: Span,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ICmp {
    Eq,
    Neq,
    Slt,
    Sgte,
    Sgt,
    Slte,
    Ult,
    Ugte,
    Ugt,
    Ulte,
}

impl From<ICmp> for clir::condcodes::IntCC {
    fn from(value: ICmp) -> Self {
        match value {
            ICmp::Eq => Self::Equal,
            ICmp::Neq => Self::NotEqual,
            ICmp::Slt => Self::SignedLessThan,
            ICmp::Sgte => Self::SignedGreaterThanOrEqual,
            ICmp::Sgt => Self::SignedGreaterThan,
            ICmp::Slte => Self::SignedLessThanOrEqual,
            ICmp::Ult => Self::UnsignedLessThan,
            ICmp::Ugte => Self::UnsignedGreaterThanOrEqual,
            ICmp::Ugt => Self::UnsignedGreaterThan,
            ICmp::Ulte => Self::UnsignedLessThanOrEqual,
        }
    }
}
