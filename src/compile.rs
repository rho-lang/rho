use std::{
    collections::{HashMap, HashSet},
    mem::{replace, take},
};

use thiserror::Error;

use crate::{
    asset::Asset,
    code::{
        Block, CaptureSource, Expr, Instr, InstrIndex, Literal, Op2, Stmt, ValueIndex, instr::Match,
    },
    eval::Closure,
    typing::RecordLayout,
};

#[derive(Default)]
pub struct Compile {
    // [package name => [id => value index]]
    symbols: HashMap<String, HashMap<String, ValueIndex>>,
    package_name: String,
    package_exports: HashMap<String, ValueIndex>,
    block: CompileBlock,
    outer_blocks: Vec<CompileBlock>,
}

struct CompileBlock {
    instrs: Vec<Instr>,
    promoted_indexes: HashSet<ValueIndex>,
    expr_index: ValueIndex,
    num_value: ValueIndex,
    scopes: Vec<HashMap<String, ValueIndex>>,
    loop_jump_targets: Vec<InstrIndex>,
    captures: Vec<(String, CaptureSource)>, // to be translated into Capture instructions
    closure_name_hint: String,
}

impl Default for CompileBlock {
    fn default() -> Self {
        Self {
            instrs: Default::default(),
            promoted_indexes: Default::default(),
            expr_index: 0,
            num_value: 0,
            // the outer most scope is the special "symbol table" accessed by exports and
            // imports only
            scopes: vec![Default::default()],
            loop_jump_targets: Default::default(),
            captures: Default::default(),
            closure_name_hint: Default::default(),
        }
    }
}

impl Compile {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(mut self, asset: &mut Asset) -> Closure {
        self.add(Instr::MakeUnit(0));
        self.add(Instr::Return(0));
        let block = Block {
            name: "<main>".into(),
            num_param: 0,
            num_value: self.block.num_value,
            instrs: self.block.instrs,
        };
        Closure::new(asset.add_block(block))
    }
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("duplicated package: {0}")]
    DuplicatedPackage(String),
    #[error("multiple package statement: {0}")]
    MultiplePackageStmt(String),
    #[error("break outside of loop")]
    OrphanBreak,
    #[error("continue outside of loop")]
    OrphanContinue,
    #[error("unknown intrinsic: {0}")]
    UnknownIntrinsic(String),
    #[error("unknown variable: {0}")]
    UnknownVariable(String),
    #[error("export `{0}` from nested scope")]
    InnerExport(String),
    #[error("multiple export: {0}")]
    MultipleExport(String),
    #[error("unknown package: {0}")]
    UnknownPackage(String),
    #[error("invalid import from package `{0}`: {1}")]
    InvalidImport(String, String),
}

impl Compile {
    pub fn input(&mut self, stmts: Vec<Stmt>, asset: &mut Asset) -> Result<(), CompileError> {
        self.block.scopes.push(Default::default());
        for stmt in stmts {
            self.input_stmt(stmt, asset)?
        }
        self.block.scopes.pop();
        assert_eq!(self.block.scopes.len(), 1); // symbol table is left
        assert!(self.block.loop_jump_targets.is_empty());
        assert!(self.block.captures.is_empty()); // should always be
        assert!(self.block.closure_name_hint.is_empty());
        assert!(self.outer_blocks.is_empty());

        let package_exports = take(&mut self.package_exports);
        let mut package_name = take(&mut self.package_name);
        if package_name.is_empty() {
            package_name = "<main>".into()
        }
        for (name, &value_index) in &package_exports {
            self.bind(format!("symbol@{package_name}:{name}"), value_index)
        }

        let replaced = self.symbols.insert(package_name.clone(), package_exports);
        if replaced.is_some() {
            return Err(CompileError::DuplicatedPackage(package_name));
        }
        Ok(())
    }

    // convention: input_stmt `add` instruction(s) that use values indexed from
    // `self.current_block.expr_index` onward as scratch pad. after input_stmt
    // returns, `self.current_block.expr_index` >= the point it was called
    fn input_stmt(&mut self, stmt: Stmt, asset: &mut Asset) -> Result<(), CompileError> {
        let expr_index = self.block.expr_index;
        match stmt {
            Stmt::Package(name) => {
                let replaced = replace(&mut self.package_name, name);
                if !replaced.is_empty() {
                    return Err(CompileError::MultiplePackageStmt(replaced));
                }
            }
            Stmt::Export(id) => {
                if !self.outer_blocks.is_empty() {
                    return Err(CompileError::InnerExport(id));
                }
                // currently, exports must happen at the outermost scope to the package, i.e.,
                // the "global" scope to the package, or the exported value may be override by
                // later instructions
                // this restriction can be relaxed by copying the value to somewhere "safe". if
                // necessary can implement that later
                // the `nth(1)` is because the 0-th scope is the symbol table
                let Some(&value_index) = self.block.scopes.iter().nth(1).unwrap().get(&id) else {
                    return Err(if self.var(&id).is_some() {
                        CompileError::InnerExport(id)
                    } else {
                        CompileError::UnknownVariable(id)
                    });
                };
                let replaced = self.package_exports.insert(id.clone(), value_index);
                if replaced.is_some() {
                    return Err(CompileError::MultipleExport(id));
                }
            }

            // for simple one pass compilation, loop is translated into
            // +0   Jump +2     # Continue target
            // +1   Jump +n     # Break target
            // ...              # Loop expr
            // +x   Jump +0     # a Continue
            // ...
            // +y   Jump +1     # a Break
            // ...
            // +n-1 Jump +0     # normal loopback
            // +n   ...         # first instruction after Loop
            Stmt::Loop(expr) => {
                let jump_target = self.block.instrs.len();
                self.add(Instr::Jump(jump_target + 2, None));
                self.add(Instr::Jump(usize::MAX, None)); // placeholder
                self.block.loop_jump_targets.push(jump_target);
                self.input_expr(expr, asset)?;
                self.block.loop_jump_targets.pop();
                self.add(Instr::Jump(jump_target, None));
                let after_target = self.block.instrs.len();
                let Instr::Jump(target, _) = &mut self.block.instrs[jump_target + 1] else {
                    unreachable!()
                };
                *target = after_target
            }
            Stmt::Break => {
                let Some(&jump_target) = self.block.loop_jump_targets.last() else {
                    return Err(CompileError::OrphanBreak);
                };
                self.add(Instr::Jump(jump_target + 1, None))
            }
            Stmt::Continue => {
                let Some(&jump_target) = self.block.loop_jump_targets.last() else {
                    return Err(CompileError::OrphanContinue);
                };
                self.add(Instr::Jump(jump_target, None))
            }

            Stmt::Intrinsic(intrinsic) => {
                let Some(&native_fn) = asset.intrinsics.get(&intrinsic.id) else {
                    return Err(CompileError::UnknownIntrinsic(intrinsic.id));
                };
                let num_binding = intrinsic.bindings.len();
                let num_arg = intrinsic.args.len();

                // first evaluate arguments without destination ids in scope
                for (i, expr) in intrinsic.args.into_iter().enumerate() {
                    self.block.expr_index = expr_index + num_binding + i;
                    self.input_expr(expr, asset)?
                }
                for (i, id) in intrinsic.bindings.into_iter().enumerate() {
                    self.bind(id, expr_index + i)
                }
                self.add(Instr::Intrinsic(
                    native_fn,
                    (expr_index..expr_index + num_binding + num_arg).collect(),
                ))
            }

            Stmt::Bind(id, expr) => {
                self.block.closure_name_hint = id.clone();
                self.input_expr(expr, asset)?;
                self.block.closure_name_hint.clear();
                self.bind(id, expr_index);
                self.block.expr_index = expr_index + 1
            }
            Stmt::Mut(id, expr) => {
                self.block.closure_name_hint = id.clone();
                self.input_expr(expr, asset)?;
                self.block.closure_name_hint.clear();
                // i wanted to do one self.add... but hold back myself so i can use multiple
                // instructions in some branches in the future
                if let Some(value_index) = self.var(&id) {
                    if !self.block.promoted_indexes.contains(&value_index) {
                        self.add(Instr::Copy(value_index, expr_index))
                    } else {
                        self.add(Instr::SetPromoted(value_index, expr_index))
                    }
                } else if let Some(captured_index) = self.try_capture(&id) {
                    self.add(Instr::SetCaptured(captured_index, expr_index))
                } else {
                    return Err(CompileError::UnknownVariable(id));
                }
            }
            Stmt::MutAttr(record_expr, attr, expr) => {
                self.input_expr(record_expr, asset)?;
                self.block.expr_index = expr_index + 1;
                self.input_expr(expr, asset)?;
                self.add(Instr::SetAttr(
                    expr_index,
                    asset.intern(attr),
                    expr_index + 1,
                ))
            }

            Stmt::Return(expr) => {
                self.input_expr(expr, asset)?;
                self.add(Instr::Return(expr_index))
            }
            Stmt::Wait(expr) => {
                self.input_expr(expr, asset)?;
                self.add(Instr::Wait(expr_index))
            }
            Stmt::Notify(expr) => {
                self.input_expr(expr, asset)?;
                self.add(Instr::Notify(expr_index))
            }
            Stmt::Spawn(expr) => {
                self.input_expr(expr, asset)?;
                self.add(Instr::Spawn(expr_index))
            }
            Stmt::Expr(expr) => self.input_expr(expr, asset)?,
        }
        Ok(())
    }

    // convention: input_expr `add` instruction(s) that produce expression's value
    // at `self.current_block.expr_index` at the point input_expr is called. when
    // input_expr returns, `self.current_block.expr_index` >= when it was called
    fn input_expr(&mut self, expr: Expr, asset: &mut Asset) -> Result<(), CompileError> {
        let expr_index = self.block.expr_index;
        self.block.num_value = self.block.num_value.max(expr_index + 1);
        match expr {
            Expr::Import(name, id) => {
                let Some(package_exports) = self.symbols.get(&name) else {
                    return Err(CompileError::UnknownPackage(name));
                };
                if !package_exports.contains_key(&id) {
                    return Err(CompileError::InvalidImport(name, id));
                };
                // pretty hacky, but seems nothing wrong to me (?)
                // although package exports[&id] would exactly be the value index we are looking
                // for, if we are compiling from an inner CompileBlock, we need to go through
                // the capturing procedure, which is what the Expr::Var compilation has been
                // implementing
                self.input_expr(Expr::Var(format!("symbol@{name}:{id}")), asset)?
            }

            Expr::Func(func) => {
                let num_param = func.params.len();

                self.outer_blocks.push(take(&mut self.block));
                // start to work on a nested current block. nest the code as a visual
                // implication (without any real functionality)
                {
                    self.block.scopes.push(Default::default());
                    for id in func.params {
                        self.bind(id, self.block.expr_index);
                        self.block.expr_index += 1
                    }
                    let expr_index = self.block.expr_index;
                    self.input_expr(*func.body, asset)?;
                    self.add(Instr::Return(expr_index));
                }
                let func_block = replace(&mut self.block, self.outer_blocks.pop().unwrap());

                let mut name = take(&mut self.block.closure_name_hint);
                if name.is_empty() {
                    name = "(unnamed)".into()
                }
                let block = Block {
                    name: format!("{}::{name}", self.package_name),
                    num_param,
                    num_value: func_block.num_value,
                    instrs: func_block.instrs,
                };
                let block_id = asset.add_block(block);
                self.add(Instr::MakeClosure(expr_index, block_id));
                for (_, capture_source) in func_block.captures {
                    if let &CaptureSource::Original(index) = &capture_source
                        && self.block.promoted_indexes.insert(index)
                    {
                        self.add(Instr::Promote(index))
                    }
                    self.add(Instr::Capture(expr_index, capture_source))
                }
            }
            Expr::Call(closure, args) => {
                self.input_expr(*closure, asset)?;
                let num_arg = args.len();
                for (i, arg) in args.into_iter().enumerate() {
                    self.block.expr_index = expr_index + 1 + i;
                    self.input_expr(arg, asset)?
                }
                self.add(Instr::Call(
                    expr_index,
                    expr_index,
                    (expr_index + 1..=expr_index + num_arg).collect(),
                ))
            }

            Expr::Match(match_expr) => {
                self.input_expr(match_expr.scrutinee, asset)?;
                let mut hit_jumps = Vec::new();
                for (pattern, expr) in match_expr.cases {
                    self.block.expr_index = expr_index + 1;
                    self.input_expr(pattern, asset)?;
                    hit_jumps.push((self.block.instrs.len(), expr));
                    self.add(Instr::Jump(
                        InstrIndex::MAX, // placeholder
                        Some(Match {
                            scrutinee: expr_index,
                            pattern: expr_index + 1,
                        }),
                    ));
                }
                self.add(Instr::MakeUnit(expr_index));
                let case_end_target = self.block.instrs.len();
                self.add(Instr::Jump(InstrIndex::MAX, None)); // placeholder

                for (jump_index, expr) in hit_jumps {
                    let target = self.block.instrs.len();
                    let Instr::Jump(index, _) = &mut self.block.instrs[jump_index] else {
                        unreachable!()
                    };
                    *index = target;

                    self.block.expr_index = expr_index;
                    self.input_expr(expr, asset)?;
                    self.add(Instr::Jump(case_end_target, None))
                }
                let after_target = self.block.instrs.len();
                let Instr::Jump(target, _) = &mut self.block.instrs[case_end_target] else {
                    unreachable!()
                };
                *target = after_target
            }

            Expr::Var(id) => {
                if let Some(value_index) = self.var(&id) {
                    self.add(Instr::Copy(expr_index, value_index));
                    if self.block.promoted_indexes.contains(&value_index) {
                        self.add(Instr::Demote(expr_index))
                    }
                } else if let Some(captured_index) = self.try_capture(&id) {
                    self.add(Instr::GetCaptured(expr_index, captured_index))
                } else {
                    return Err(CompileError::UnknownVariable(id));
                }
            }

            Expr::Compound(stmts, expr) => {
                self.block.scopes.push(Default::default());
                for stmt in stmts {
                    self.input_stmt(stmt, asset)?
                }
                let value_index = self.block.expr_index;
                self.input_expr(*expr, asset)?;
                self.add(Instr::Copy(expr_index, value_index));
                self.block.scopes.pop().unwrap();
            }

            Expr::Type(attrs) => {
                let attrs = attrs.into_iter().map(|attr| asset.intern(attr)).collect();
                self.add(Instr::MakeRecordType(expr_index, RecordLayout(attrs)))
            }
            Expr::Record(type_expr, attr_exprs) => {
                self.input_expr(*type_expr, asset)?;
                let mut attrs = Vec::new();
                for (i, (name, expr)) in attr_exprs.into_iter().enumerate() {
                    self.block.expr_index = expr_index + 1 + i;
                    attrs.push((asset.intern(name), self.block.expr_index));
                    self.input_expr(expr, asset)?
                }
                self.add(Instr::MakeRecord(expr_index, expr_index, attrs))
            }

            Expr::Op(op, args) => {
                let num_arg = args.len();
                for (i, arg) in args.into_iter().enumerate() {
                    self.block.expr_index = expr_index + i;
                    self.input_expr(arg, asset)?;
                }
                match num_arg {
                    2 => {
                        let op = match &*op {
                            "+" => Op2::Add,
                            "-" => Op2::Sub,
                            "==" => Op2::Eq,
                            "!=" => Op2::Ne,
                            "*" => Op2::Mul,
                            "/" => Op2::Div,
                            "%" => Op2::Rem,
                            "<" => Op2::Lt,
                            ">" => Op2::Gt,
                            "<=" => Op2::Le,
                            ">=" => Op2::Ge,
                            _ => unimplemented!(),
                        };
                        self.add(Instr::Op2(expr_index, op, expr_index, expr_index + 1))
                    }
                    _ => unimplemented!(),
                }
            }
            Expr::GetAttr(expr, attr) => {
                self.input_expr(*expr, asset)?;
                self.add(Instr::GetAttr(expr_index, expr_index, asset.intern(attr)))
            }

            Expr::Literal(Literal::Unit) => self.add(Instr::MakeUnit(expr_index)),
            Expr::Literal(Literal::String(string)) => {
                self.add(Instr::MakeString(expr_index, string))
            }
            Expr::Literal(Literal::Signal) => self.add(Instr::MakeSignal(expr_index)),
            Expr::Literal(Literal::I32(value)) => self.add(Instr::MakeI32(expr_index, value)),
        }
        Ok(())
    }

    fn add(&mut self, instr: Instr) {
        self.block.instrs.push(instr)
    }

    // want to do def() and use(), but use is keyword
    fn bind(&mut self, id: String, value_index: ValueIndex) {
        self.block
            .scopes
            .last_mut()
            .unwrap()
            .insert(id, value_index);
    }

    fn var(&self, id: &str) -> Option<ValueIndex> {
        Self::var_impl(id, &self.block)
    }

    fn var_impl(id: &str, block: &CompileBlock) -> Option<ValueIndex> {
        for scope in block.scopes.iter().rev() {
            if let Some(&value_index) = scope.get(id) {
                return Some(value_index);
            }
        }
        None
    }

    fn try_capture(&mut self, id: &str) -> Option<usize> {
        for (index, (captured_id, _)) in self.block.captures.iter().enumerate() {
            if captured_id == id {
                return Some(index);
            }
        }

        if self.outer_blocks.is_empty() {
            return None;
        }
        let index = self.outer_blocks.len() - 1;
        let source = Self::try_capture_impl(id, &mut self.outer_blocks, index)?;

        let captured_index = self.block.captures.len();
        self.block.captures.push((id.into(), source));
        Some(captured_index)
    }

    fn try_capture_impl(
        id: &str,
        outer_blocks: &mut [CompileBlock],
        index: usize,
    ) -> Option<CaptureSource> {
        if let Some(value_index) = Self::var_impl(id, &outer_blocks[index]) {
            return Some(CaptureSource::Original(value_index));
        }

        for (captured_index, (captured_id, _)) in outer_blocks[index].captures.iter().enumerate() {
            if captured_id == id {
                return Some(CaptureSource::Transitive(captured_index));
            }
        }

        if index == 0 {
            return None;
        }
        let source = Self::try_capture_impl(id, outer_blocks, index - 1)?;

        let captured_index = outer_blocks[index].captures.len();
        outer_blocks[captured_index]
            .captures
            .push((id.into(), source));
        Some(CaptureSource::Transitive(captured_index))
    }
}
