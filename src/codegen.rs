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
use cranelift::prelude::isa::OwnedTargetIsa;
use rustc_hash::FxHashMap;

pub struct Module {
    ctx: clb::Context,
    func_ctx: clf::FunctionBuilderContext,
    gen_ctx: Context,
}

struct Context {
    numbers: FxHashMap<InternedStr, i64>,
    strings: FxHashMap<InternedStr, clm::DataId>,
    module: clo::ObjectModule,
    word_type: clir::Type,
    globals: FxHashMap<InternedStr, GlobalInfo>,
    diag: Rc<Diagnostics>,
}

struct Function<'f> {
    builder: clf::FunctionBuilder<'f>,
    labels: FxHashMap<InternedStr, LabelInfo>,
    unused_labels: FxHashMap<InternedStr, Span>,
    autos: FxHashMap<InternedStr, AutoInfo>,
    signatures: FxHashMap<usize, clir::SigRef>,
    return_block: clir::Block,
    ctx: &'f mut Context,
    name: &'f Name,
}

struct LabelInfo {
    block: clir::Block,
    span: Span,
}

struct GlobalInfo {
    id: clm::FuncOrDataId,
    is_rvalue: bool,
    span: Span,
}

impl Context {
    pub fn new(isa: OwnedTargetIsa, path: &Path, diag: Rc<Diagnostics>) -> Self {
        let word_type = isa.pointer_type();
        let builder = clo::ObjectBuilder::new(
            isa,
            path.as_os_str().as_encoded_bytes(),
            clm::default_libcall_names(),
        )
        .unwrap();
        let module = clo::ObjectModule::new(builder);

        Self {
            numbers: FxHashMap::default(),
            strings: FxHashMap::default(),
            module,
            word_type,
            globals: FxHashMap::default(),
            diag,
        }
    }

    fn create_number(&mut self, value: InternedStr) -> i64 {
        *self.numbers.entry(value).or_insert_with(|| {
            // FIXME: implement custom parsing
            let bytes = value.display();
            debug_assert!(bytes.is_ascii());
            // SAFETY: number only consists of digits, it maybe not true for other literals
            let str = unsafe { bytes.to_str_unchecked() };
            str.parse::<i64>().unwrap()
        })
    }

    fn create_string(&mut self, value: InternedStr, span: Span) -> clm::DataId {
        *self.strings.entry(value).or_insert_with(|| {
            let data_id = self.module.declare_anonymous_data(true, false).unwrap();
            let mut desc = clm::DataDescription::new();
            let mut data = match unescape(value.display()) {
                Ok(data) => data,
                Err(place) => {
                    let start = span.start + place as u32;
                    self.diag
                        .error(Span::new(start, start + 1), "unknown escape symbol")
                        .finish();
                    return clm::DataId::from_u32(0);
                }
            };
            data.push(b'\0');
            desc.define(data.into_boxed_slice());
            self.module.define_data(data_id, &desc).unwrap();
            data_id
        })
    }

    fn create_char(&mut self, value: InternedStr, span: Span) -> u16 {
        let bytes = value.display();
        match bytes.len() {
            0 => {
                self.diag
                    .error(span, "empty character is not supported")
                    .finish();
                0
            }
            1 => bytes[0] as u16,
            2 => {
                if bytes[0] != b'*' {
                    (bytes[0] as u16) << u8::BITS | bytes[1] as u16
                } else {
                    let c = unescape_char(bytes[1]);
                    if c.is_none() {
                        self.diag.error(span, "unknown escape symbol").finish();
                    }
                    c.map(u16::from).unwrap_or(0)
                }
            }
            _ => {
                self.diag
                    .error(span, "character constant is longer than 2 symbols")
                    .finish();
                0
            }
        }
    }
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

        let gen_ctx = Context::new(isa, path, diag);
        let ctx = gen_ctx.module.make_context();
        let func_ctx = clf::FunctionBuilderContext::new();

        Self {
            func_ctx,
            ctx,
            gen_ctx,
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

    fn module(&mut self) -> &mut clo::ObjectModule {
        &mut self.gen_ctx.module
    }

    fn run_global_pass_on(&mut self, def: &DefAst) {
        let global_name = def.name.value.display().to_str().unwrap();
        let (id, is_rvalue) = match &def.kind {
            DefKind::Vector { size, .. } => {
                let data_id = self
                    .module()
                    .declare_data(global_name, clm::Linkage::Export, true, false)
                    .unwrap();

                let is_rvalue = !matches!(size, VecSize::Undef);
                (clm::FuncOrDataId::Data(data_id), is_rvalue)
            }
            DefKind::Function { params, .. } => {
                let params = &params.params;
                let mut sig = self.module().make_signature();
                sig.params.extend(std::iter::repeat_n(
                    clir::AbiParam::new(self.gen_ctx.word_type),
                    params.len(),
                ));
                sig.returns
                    .push(clir::AbiParam::new(self.gen_ctx.word_type));

                let func_id = self
                    .module()
                    .declare_function(global_name, clm::Linkage::Export, &sig)
                    .unwrap();
                (clm::FuncOrDataId::Func(func_id), true)
            }
        };
        self.gen_ctx
            .globals
            .entry(def.name.value)
            .and_modify(|global| {
                self.gen_ctx
                    .diag
                    .error(
                        def.name.span,
                        format!("redefinition of global name {}", def.name.value.display()),
                    )
                    .add_label(global.span, "first defined here")
                    .finish();
            })
            .or_insert(GlobalInfo {
                id,
                is_rvalue,
                span: def.name.span,
            });
    }

    fn run_local_pass_on(&mut self, def: &DefAst, print: bool) {
        let global = self
            .gen_ctx
            .globals
            .get(&def.name.value)
            .expect("Run local pass to define globals");
        match &def.kind {
            DefKind::Vector { list, size } => {
                let data_id = match global.id {
                    clm::FuncOrDataId::Data(data_id) => data_id,
                    _ => panic!("Expected data id in globals table"),
                };

                let size = match size {
                    VecSize::Undef => 1,
                    VecSize::Empty(_) => 0,
                    VecSize::Def { lit, .. } => match lit.kind {
                        LiteralKind::Number => self.gen_ctx.create_number(lit.value) as usize,
                        _ => {
                            self.gen_ctx
                                .diag
                                .error(lit.span, "vector size must be integer")
                                .finish();
                            0
                        }
                    },
                };
                let size = size.max(list.len());
                let size_in_bytes = size * self.gen_ctx.word_type.bytes() as usize;

                let mut data = clm::DataDescription::new();
                data.set_align(self.gen_ctx.word_type.bytes() as u64);
                if list.is_empty() {
                    data.define_zeroinit(size_in_bytes);
                } else {
                    let mut bytes = Vec::with_capacity(size_in_bytes);
                    for ival in list {
                        match ival {
                            ImmVal::Const(lit) => match lit.kind {
                                LiteralKind::Number => {
                                    let num = self.gen_ctx.create_number(lit.value);

                                    bytes.extend_from_slice(&match self
                                        .gen_ctx
                                        .module
                                        .isa()
                                        .endianness()
                                    {
                                        clir::Endianness::Little => num.to_le_bytes(),
                                        clir::Endianness::Big => num.to_be_bytes(),
                                    });
                                }
                                LiteralKind::Char => {
                                    let char = self.gen_ctx.create_char(lit.value, lit.span) as i64;

                                    bytes.extend_from_slice(&match self
                                        .gen_ctx
                                        .module
                                        .isa()
                                        .endianness()
                                    {
                                        clir::Endianness::Little => char.to_le_bytes(),
                                        clir::Endianness::Big => char.to_be_bytes(),
                                    });
                                }
                                LiteralKind::String => {
                                    let data_id = self.gen_ctx.create_string(lit.value, lit.span);
                                    let gv = self.module().declare_data_in_data(data_id, &mut data);
                                    data.write_data_addr(bytes.len() as u32, gv, 0);
                                    bytes.extend(std::iter::repeat_n(
                                        0,
                                        self.gen_ctx.word_type.bytes() as usize,
                                    ));
                                }
                            },
                            ImmVal::Name(name) => {
                                if let Some(func_or_data) = self.gen_ctx.globals.get(&name.value) {
                                    match func_or_data.id {
                                        clm::FuncOrDataId::Func(func_id) => {
                                            let func_ref = self
                                                .module()
                                                .declare_func_in_data(func_id, &mut data);
                                            data.write_function_addr(bytes.len() as u32, func_ref);
                                        }
                                        clm::FuncOrDataId::Data(data_id) => {
                                            let gv = self
                                                .module()
                                                .declare_data_in_data(data_id, &mut data);
                                            data.write_data_addr(bytes.len() as u32, gv, 0);
                                        }
                                    }
                                    bytes.extend(std::iter::repeat_n(
                                        0,
                                        self.gen_ctx.word_type.bytes() as usize,
                                    ));
                                } else {
                                    self.gen_ctx
                                        .diag
                                        .error(name.span, "undefined global name")
                                        .finish();
                                }
                            }
                        }
                    }
                    debug_assert_eq!(bytes.len(), size_in_bytes);
                    data.define(bytes.into_boxed_slice());
                };

                self.module().define_data(data_id, &data).unwrap();
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
                    .module()
                    .declarations()
                    .get_function_decl(func_id)
                    .signature
                    .clone();

                let cfunc = Function::new(self, &def.name);
                cfunc.build(params, body);

                if self.gen_ctx.diag.has_errors() {
                    self.func_ctx = clf::FunctionBuilderContext::new();
                    return;
                }

                self.gen_ctx
                    .module
                    .define_function(func_id, &mut self.ctx)
                    .unwrap();
                if print {
                    println!("{}", self.ctx.func.display());
                }
                clb::verify_function(&self.ctx.func, self.gen_ctx.module.isa().flags()).unwrap();
            }
        }
    }

    pub fn finish(self) -> clo::ObjectProduct {
        self.gen_ctx.module.finish()
    }
}

impl<'f> Function<'f> {
    fn new(module: &'f mut Module, name: &'f Name) -> Self {
        let builder = clf::FunctionBuilder::new(&mut module.ctx.func, &mut module.func_ctx);

        Self {
            builder,
            labels: FxHashMap::default(),
            unused_labels: FxHashMap::default(),
            autos: FxHashMap::default(),
            signatures: FxHashMap::default(),
            return_block: clir::Block::from_u32(0),
            ctx: &mut module.gen_ctx,
            name,
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

        for (value, span) in self.unused_labels.drain() {
            self.ctx
                .diag
                .error(span, format!("undefined label '{}'", value.display()))
                .finish();
        }

        if self.ctx.diag.has_errors() {
            return;
        }

        if let Some(block) = self.builder.current_block() {
            if self.builder.func.block_successors(block).next().is_none() {
                let zero = self.builder.ins().iconst(self.ctx.word_type, 0);
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

    fn declare_auto(&mut self, name: &Name, value: clir::Value, is_rvalue: bool) {
        self.autos
            .entry(name.value)
            .and_modify(|info| {
                self.ctx
                    .diag
                    .error(
                        name.span,
                        format!("redefinition of name {}", name.value.display()),
                    )
                    .add_label(info.span, "first defined here")
                    .finish();
            })
            .or_insert(AutoInfo {
                value,
                is_rvalue,
                span: name.span,
            });
    }

    // TODO: alloc with constant
    fn create_auto(&mut self, name: &Name, size: u32) {
        let slot = self
            .builder
            .create_sized_stack_slot(clir::StackSlotData::new(
                clir::StackSlotKind::ExplicitSlot,
                self.ctx.word_type.bytes() * size,
                self.ctx.word_type.bytes().ilog2() as u8,
            ));

        let addr = self.builder.ins().stack_addr(self.ctx.word_type, slot, 0);
        self.declare_auto(name, addr, size > 1);
    }

    fn create_param(&mut self, name: &Name, value: clir::Value) {
        let slot = self
            .builder
            .create_sized_stack_slot(clir::StackSlotData::new(
                clir::StackSlotKind::ExplicitSlot,
                self.ctx.word_type.bytes(),
                self.ctx.word_type.bytes().ilog2() as u8,
            ));

        self.builder.ins().stack_store(value, slot, 0);

        let addr = self.builder.ins().stack_addr(self.ctx.word_type, slot, 0);
        self.declare_auto(name, addr, false);
    }

    fn create_literal(&mut self, literal: &Literal) -> clir::Value {
        match literal.kind {
            LiteralKind::Number => {
                let num = self.ctx.create_number(literal.value);
                self.builder.ins().iconst(self.ctx.word_type, num)
            }
            LiteralKind::Char => {
                let char = self.ctx.create_char(literal.value, literal.span) as i64;
                self.builder.ins().iconst(self.ctx.word_type, char)
            }
            LiteralKind::String => {
                let data_id = self.ctx.create_string(literal.value, literal.span);
                let gv = self
                    .ctx
                    .module
                    .declare_data_in_func(data_id, self.builder.func);
                self.builder.ins().global_value(self.ctx.word_type, gv)
            }
        }
    }

    fn make_rvalue(&mut self, value: &mut Value) {
        if value.is_lvalue {
            value.inner = self.builder.ins().load(
                self.ctx.word_type,
                clir::MemFlags::trusted(),
                value.inner,
                0,
            );
            value.is_lvalue = false;
        }
    }

    fn try_store(&mut self, mut value: Value, addr: Value) -> Option<Value> {
        if !addr.is_lvalue {
            return None;
        }
        self.make_rvalue(&mut value);
        self.builder
            .ins()
            .store(clir::MemFlags::trusted(), value.inner, addr.inner, 0);
        Some(value)
    }

    fn incdec(&mut self, x: Value, imm: i64, is_post: bool) -> Option<clir::Value> {
        if !x.is_lvalue {
            return None;
        }

        let a = self
            .builder
            .ins()
            .load(self.ctx.word_type, clir::MemFlags::trusted(), x.inner, 0);
        let b = self.builder.ins().iadd_imm(a, imm);
        self.builder
            .ins()
            .store(clir::MemFlags::trusted(), b, x.inner, 0);
        Some(if is_post { a } else { b })
    }

    fn expected_lvalue(&self, unary: &UnaryExpr) {
        self.ctx
            .diag
            .error(
                unary.op.span,
                format!("'{}' expects lvalue as operand", unary.op.kind),
            )
            .add_label(unary.expr.span, "found rvalue")
            .finish();
    }

    #[inline]
    fn build_cmp(&mut self, cmp: ICmp, lhs: clir::Value, rhs: clir::Value) -> clir::Value {
        let bool = self.builder.ins().icmp(cmp, lhs, rhs);
        self.builder.ins().uextend(self.ctx.word_type, bool)
    }

    fn apply_binary(
        &mut self,
        binop: BinOpKind,
        lhs: clir::Value,
        rhs: clir::Value,
    ) -> clir::Value {
        let ins = self.builder.ins();
        match binop {
            BinOpKind::Or => ins.bor(lhs, rhs),
            BinOpKind::And => ins.band(lhs, rhs),
            BinOpKind::Eq => self.build_cmp(ICmp::Eq, lhs, rhs),
            BinOpKind::Neq => self.build_cmp(ICmp::Neq, lhs, rhs),
            BinOpKind::Lt => self.build_cmp(ICmp::Slt, lhs, rhs),
            BinOpKind::LtEq => self.build_cmp(ICmp::Slte, lhs, rhs),
            BinOpKind::Gt => self.build_cmp(ICmp::Sgt, lhs, rhs),
            BinOpKind::GtEq => self.build_cmp(ICmp::Sgte, lhs, rhs),
            BinOpKind::Shl => ins.ishl(lhs, rhs),
            BinOpKind::Shr => ins.ushr(lhs, rhs),
            BinOpKind::Add => ins.iadd(lhs, rhs),
            BinOpKind::Sub => ins.isub(lhs, rhs),
            BinOpKind::Rem => ins.srem(lhs, rhs),
            BinOpKind::Mul => ins.imul(lhs, rhs),
            BinOpKind::Div => ins.sdiv(lhs, rhs),
        }
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
            if let Some(lit) = &decl.value {
                if matches!(lit.kind, LiteralKind::Number) {
                    let size = self.ctx.create_number(lit.value);
                    self.create_auto(&decl.name, size as u32);
                } else {
                    self.ctx
                        .diag
                        .error(lit.span, "auto array size must be integer")
                        .finish();
                }
            } else {
                self.create_auto(&decl.name, 1);
            }
        }
    }

    fn visit_extrn(&mut self, extrn: &ExtrnStmt) {
        for name in &extrn.names {
            if let Some(global) = self.ctx.globals.get(&name.value) {
                match global.id {
                    clm::FuncOrDataId::Func(func_id) => {
                        let func_ref = self
                            .ctx
                            .module
                            .declare_func_in_func(func_id, self.builder.func);
                        let addr = self.builder.ins().func_addr(self.ctx.word_type, func_ref);
                        self.declare_auto(name, addr, global.is_rvalue);
                    }
                    clm::FuncOrDataId::Data(data_id) => {
                        let gv = self
                            .ctx
                            .module
                            .declare_data_in_func(data_id, self.builder.func);
                        let value = self.builder.ins().global_value(self.ctx.word_type, gv);
                        self.declare_auto(name, value, global.is_rvalue);
                    }
                }
            } else {
                let global_name = name.value.display().to_str().unwrap();
                let mut signature = self.ctx.module.make_signature();
                signature
                    .params
                    .push(clir::AbiParam::new(self.ctx.word_type));
                signature
                    .returns
                    .push(clir::AbiParam::new(self.ctx.word_type));
                let func_id = self
                    .ctx
                    .module
                    .declare_function(global_name, clm::Linkage::Import, &signature)
                    .unwrap();
                let func_ref = self
                    .ctx
                    .module
                    .declare_func_in_func(func_id, self.builder.func);
                let addr = self.builder.ins().func_addr(self.ctx.word_type, func_ref);
                self.declare_auto(name, addr, true);
            }
        }
    }

    fn visit_label(&mut self, label: &LabelStmt) {
        let label_block = self
            .labels
            .entry(label.name.value)
            .and_modify(|info| {
                if info.span != Span::empty() {
                    self.ctx
                        .diag
                        .error(
                            label.name.span,
                            format!("redefinition of label '{}'", label.name.value.display()),
                        )
                        .add_label(info.span, "first defined here")
                        .finish();
                } else {
                    self.unused_labels.remove(&label.name.value);
                }
            })
            .or_insert_with(|| LabelInfo {
                block: self.builder.create_block(),
                span: label.name.span,
            })
            .block;
        self.builder.ins().jump(label_block, &[]);
        self.builder.switch_to_block(label_block);
        self.visit_stmt(&label.stmt);
    }

    fn visit_goto(&mut self, goto: &GotoStmt) {
        let label_block = self
            .labels
            .entry(goto.label.value)
            .or_insert_with(|| {
                self.unused_labels.insert(goto.label.value, goto.label.span);
                LabelInfo {
                    block: self.builder.create_block(),
                    span: Span::empty(),
                }
            })
            .block;
        self.builder.ins().jump(label_block, &[]);
        let after_goto = self.builder.create_block();
        self.builder.switch_to_block(after_goto);
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
            None => self.builder.ins().iconst(self.ctx.word_type, 0),
        };
        self.builder
            .ins()
            .jump(self.return_block, &[clir::BlockArg::Value(val)]);
        let after_return = self.builder.create_block();
        self.builder.switch_to_block(after_return);
    }

    fn visit_cond(&mut self, cond: &CondStmt) {
        if let Some(mut condition) = self.visit_expr(&cond.cond) {
            let then_bb = self.builder.create_block();
            let end_bb = self.builder.create_block();

            let else_or_end_bb = if cond.else_stmt.is_some() {
                self.builder.create_block()
            } else {
                end_bb
            };
            self.make_rvalue(&mut condition);
            self.builder
                .ins()
                .brif(condition.inner, then_bb, &[], else_or_end_bb, &[]);

            // then
            self.builder.switch_to_block(then_bb);
            self.visit_stmt(&cond.then_stmt);
            self.builder.ins().jump(end_bb, &[]);

            // else
            if let Some(else_stmt) = &cond.else_stmt {
                self.builder.switch_to_block(else_or_end_bb);
                self.visit_stmt(else_stmt);
                self.builder.ins().jump(end_bb, &[]);
            }

            self.builder.switch_to_block(end_bb);
        }
    }

    fn visit_while(&mut self, while_: &WhileStmt) {
        let cond_bb = self.builder.create_block();
        let body_bb = self.builder.create_block();
        let end_bb = self.builder.create_block();

        self.builder.ins().jump(cond_bb, &[]);

        // condition
        self.builder.switch_to_block(cond_bb);
        let mut cond = self
            .visit_expr(&while_.cond)
            .unwrap_or(Value::rvalue(clir::Value::from_u32(0)));
        self.make_rvalue(&mut cond);
        self.builder
            .ins()
            .brif(cond.inner, body_bb, &[], end_bb, &[]);

        // body
        self.builder.switch_to_block(body_bb);
        self.visit_stmt(&while_.stmt);
        self.builder.ins().jump(cond_bb, &[]);

        // end
        self.builder.switch_to_block(end_bb);
    }
}

impl ExprVisitor for Function<'_> {
    type Value = Option<Value>;

    fn visit_name(&mut self, name: &Name) -> Self::Value {
        if let Some(info) = self.autos.get(&name.value) {
            Some(if info.is_rvalue {
                Value::rvalue(info.value)
            } else {
                Value::lvalue(info.value)
            })
        } else {
            self.ctx
                .diag
                .error(
                    name.span,
                    format!(
                        "undefined name '{}' in function '{}'",
                        name.value.display(),
                        self.name.value.display()
                    ),
                )
                .add_label(self.name.span, "provide extrn in this function")
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
        let lhs = self.visit_expr(&assign.lhs)?;
        let rhs = self.visit_expr(&assign.rhs)?;
        if let Some(bin_op) = assign.op.kind {
            if !lhs.is_lvalue {
                return None;
            }

            let a = self.builder.ins().load(
                self.ctx.word_type,
                clir::MemFlags::trusted(),
                lhs.inner,
                0,
            );
            let b = self.apply_binary(bin_op, a, rhs.inner);
            self.builder
                .ins()
                .store(clir::MemFlags::trusted(), b, lhs.inner, 0);
            Some(Value::rvalue(b))
        } else if let Some(res) = self.try_store(rhs, lhs) {
            Some(res)
        } else {
            self.ctx
                .diag
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
            Deref => {
                self.make_rvalue(&mut expr);
                Some(Value::lvalue(expr.inner))
            }
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
        Some(Value::rvalue(self.apply_binary(binary.op.kind, lhs, rhs)))
    }

    // NOTE: addr is bytes on modern systems, but in B it is words
    fn visit_offset(&mut self, offset: &OffsetExpr) -> Self::Value {
        let mut base = self.visit_expr(&offset.base)?;
        let mut off_in_words = self.visit_expr(&offset.offset)?;
        self.make_rvalue(&mut base);
        self.make_rvalue(&mut off_in_words);

        let off = self
            .builder
            .ins()
            .imul_imm(off_in_words.inner, self.ctx.word_type.bytes() as i64);
        Some(Value::lvalue(self.builder.ins().iadd(base.inner, off)))
    }

    fn visit_ternary(&mut self, ternary: &TernaryExpr) -> Self::Value {
        let mut condition = self.visit_expr(&ternary.cond)?;
        self.make_rvalue(&mut condition);

        let then_bb = self.builder.create_block();
        let else_bb = self.builder.create_block();
        let end_bb = self.builder.create_block();

        self.make_rvalue(&mut condition);
        self.builder
            .ins()
            .brif(condition.inner, then_bb, &[], else_bb, &[]);

        // then
        self.builder.switch_to_block(then_bb);
        let mut then_val = self.visit_expr(&ternary.then_expr)?;
        self.make_rvalue(&mut then_val);
        self.builder
            .ins()
            .jump(end_bb, &[clir::BlockArg::Value(then_val.inner)]);

        // else
        self.builder.switch_to_block(else_bb);
        let mut else_val = self.visit_expr(&ternary.else_expr)?;
        self.make_rvalue(&mut else_val);
        self.builder
            .ins()
            .jump(end_bb, &[clir::BlockArg::Value(else_val.inner)]);

        self.builder.append_block_param(end_bb, self.ctx.word_type);
        self.builder.switch_to_block(end_bb);
        Some(Value::rvalue(self.builder.block_params(end_bb)[0]))
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
            let mut sig = self.ctx.module.make_signature();
            sig.params.extend(std::iter::repeat_n(
                clir::AbiParam::new(self.ctx.word_type),
                call.args.len(),
            ));
            sig.returns.push(clir::AbiParam::new(self.ctx.word_type));
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
    is_rvalue: bool,
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
