use std::{
    collections::{HashMap, HashSet},
    mem::{replace, take},
};

use thiserror::Error;

use crate::{
    asset::Asset,
    code::{Block, CaptureSource, Expr, Instr, InstrIndex, Literal, Stmt, ValueIndex},
    eval::Closure,
};

#[derive(Default)]
pub struct Compile {
    main: Vec<Instr>,
    main_num_value: usize,

    packages: HashMap<String, Package>,
    current_package_name: String,
    current_package: Package,
    current_block: CompileBlock,
    current_outer_blocks: Vec<CompileBlock>,
}

#[derive(Default)]
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

#[derive(Default)]
struct Package {
    exports: HashMap<String, ValueIndex>,
}

impl Compile {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn finish(mut self, asset: &mut Asset) -> Closure {
        self.main.push(Instr::MakeUnit(0));
        self.main.push(Instr::Return(0));
        let block = Block {
            name: "<main>".into(),
            num_param: 0,
            num_value: self.main_num_value,
            instrs: self.main,
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
        self.current_block.scopes.push(Default::default());
        for stmt in stmts {
            self.input_stmt(stmt, asset)?
        }

        assert!(self.current_outer_blocks.is_empty());
        let block = take(&mut self.current_block);
        self.main.extend(block.instrs);
        self.main_num_value = self.main_num_value.max(block.num_value);

        let mut package_name = take(&mut self.current_package_name);
        if package_name.is_empty() {
            package_name = "<main>".into()
        }
        let replaced = self
            .packages
            .insert(package_name.clone(), take(&mut self.current_package));
        if replaced.is_none() {
            Ok(())
        } else {
            Err(CompileError::DuplicatedPackage(package_name))
        }
    }

    // convention: input_stmt `add` instruction(s) that use values indexed from
    // `self.current_block.expr_index` onward as scratch pad. after input_stmt
    // returns, `self.current_block.expr_index` >= the point it was called
    fn input_stmt(&mut self, stmt: Stmt, asset: &mut Asset) -> Result<(), CompileError> {
        let expr_index = self.current_block.expr_index;
        match stmt {
            Stmt::Package(name) => {
                let replaced = replace(&mut self.current_package_name, name);
                if !replaced.is_empty() {
                    return Err(CompileError::MultiplePackageStmt(replaced));
                }
            }
            Stmt::Export(id) => {
                if !self.current_outer_blocks.is_empty() {
                    return Err(CompileError::InnerExport(id));
                }
                let Some(&value_index) = self.current_block.scopes.first().unwrap().get(&id) else {
                    return Err(if self.var(&id).is_some() {
                        CompileError::InnerExport(id)
                    } else {
                        CompileError::UnknownVariable(id)
                    });
                };
                let replaced = self.current_package.exports.insert(id.clone(), value_index);
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
                let jump_target = self.current_block.instrs.len();
                self.add(Instr::Jump(jump_target + 2, None));
                self.add(Instr::Jump(usize::MAX, None)); // placeholder
                self.current_block.loop_jump_targets.push(jump_target);
                self.input_expr(expr, asset)?;
                self.current_block.loop_jump_targets.pop();
                self.add(Instr::Jump(jump_target, None));
                let after_target = self.current_block.instrs.len() + 1;
                self.current_block.instrs[jump_target + 1] = Instr::Jump(after_target, None)
            }
            Stmt::Break => {
                let Some(&jump_target) = self.current_block.loop_jump_targets.last() else {
                    return Err(CompileError::OrphanBreak);
                };
                self.add(Instr::Jump(jump_target + 1, None))
            }
            Stmt::Continue => {
                let Some(&jump_target) = self.current_block.loop_jump_targets.last() else {
                    return Err(CompileError::OrphanContinue);
                };
                self.add(Instr::Jump(jump_target, None))
            }

            Stmt::Intrinsic(intrinsic) => {
                let Some(&native_fn) = asset.intrinsics.get(&intrinsic.id) else {
                    return Err(CompileError::UnknownIntrinsic(intrinsic.id));
                };
                let num_dst_id = intrinsic.dst_ids.len();
                let num_arg = intrinsic.args.len();

                // first evaluate arguments without destination ids in scope
                for (i, expr) in intrinsic.args.into_iter().enumerate() {
                    self.current_block.expr_index = expr_index + num_dst_id + i;
                    self.input_expr(expr, asset)?
                }
                for (i, id) in intrinsic.dst_ids.into_iter().enumerate() {
                    self.bind(id, expr_index + i)
                }
                self.add(Instr::Intrinsic(
                    native_fn,
                    (expr_index..expr_index + num_dst_id + num_arg).collect(),
                ))
            }

            Stmt::Bind(id, expr) => {
                self.current_block.closure_name_hint = id.clone();
                self.input_expr(expr, asset)?;
                self.current_block.closure_name_hint.clear();
                self.bind(id, self.current_block.expr_index);
                self.current_block.expr_index += 1;
            }
            Stmt::Mutate(id, expr) => {
                self.current_block.closure_name_hint = id.clone();
                self.input_expr(expr, asset)?;
                self.current_block.closure_name_hint.clear();
                // i wanted to do one self.add... but hold back myself so i can use multiple
                // instructions in some branches in the future
                if let Some(value_index) = self.var(&id) {
                    if !self.current_block.promoted_indexes.contains(&value_index) {
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
        let expr_index = self.current_block.expr_index;
        self.current_block.num_value = self.current_block.num_value.max(expr_index + 1);
        match expr {
            Expr::Import(name, id) => {
                let Some(package) = self.packages.get(&name) else {
                    return Err(CompileError::UnknownPackage(name));
                };
                let Some(&value_index) = package.exports.get(&id) else {
                    return Err(CompileError::InvalidImport(name, id));
                };
                self.add(Instr::Copy(expr_index, value_index))
            }

            Expr::Literal(Literal::Func(func)) => {
                let num_param = func.params.len();

                self.current_outer_blocks
                    .push(take(&mut self.current_block));
                // start to work on a nested current block. nest the code as a visual
                // implication (without any real functionality)
                {
                    self.current_block.scopes.push(Default::default());
                    for id in func.params {
                        self.bind(id, self.current_block.expr_index);
                        self.current_block.expr_index += 1
                    }
                    let expr_index = self.current_block.expr_index;
                    self.input_expr(*func.body, asset)?;
                    self.add(Instr::Return(expr_index));
                }
                let func_block = replace(
                    &mut self.current_block,
                    self.current_outer_blocks.pop().unwrap(),
                );

                let mut name = take(&mut self.current_block.closure_name_hint);
                if name.is_empty() {
                    name = "(unnamed)".into()
                }
                let block = Block {
                    name,
                    num_param,
                    num_value: func_block.num_value,
                    instrs: func_block.instrs,
                };
                let block_id = asset.add_block(block);
                self.add(Instr::MakeClosure(expr_index, block_id));
                for (_, capture_source) in func_block.captures {
                    if let &CaptureSource::Original(index) = &capture_source
                        && self.current_block.promoted_indexes.insert(index)
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
                    self.current_block.expr_index = expr_index + 1 + i;
                    self.input_expr(arg, asset)?
                }
                self.add(Instr::Call(
                    expr_index,
                    expr_index,
                    (expr_index + 1..=expr_index + num_arg).collect(),
                ))
            }

            Expr::Var(id) => {
                if let Some(value_index) = self.var(&id) {
                    self.add(Instr::Copy(expr_index, value_index));
                    if self.current_block.promoted_indexes.contains(&value_index) {
                        self.add(Instr::Demote(expr_index))
                    }
                } else if let Some(captured_index) = self.try_capture(&id) {
                    self.add(Instr::GetCaptured(expr_index, captured_index))
                } else {
                    return Err(CompileError::UnknownVariable(id));
                }
            }

            Expr::Compound(stmts, expr) => {
                self.current_block.scopes.push(Default::default());
                for stmt in stmts {
                    self.input_stmt(stmt, asset)?
                }
                let value_index = self.current_block.expr_index;
                self.input_expr(*expr, asset)?;
                self.add(Instr::Copy(expr_index, value_index));
                self.current_block.scopes.pop().unwrap();
            }

            Expr::Match(_) => todo!(),

            Expr::Literal(Literal::Unit) => self.add(Instr::MakeUnit(expr_index)),
            Expr::Literal(Literal::String(string)) => {
                self.add(Instr::MakeString(expr_index, string))
            }
            Expr::Future => self.add(Instr::MakeFuture(expr_index)),
        }
        Ok(())
    }

    fn add(&mut self, instr: Instr) {
        self.current_block.instrs.push(instr)
    }

    // want to do def() and use(), but use is keyword
    fn bind(&mut self, id: String, value_index: ValueIndex) {
        self.current_block
            .scopes
            .last_mut()
            .unwrap()
            .insert(id, value_index);
    }

    fn var(&self, id: &str) -> Option<ValueIndex> {
        Self::var_impl(id, &self.current_block)
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
        for (index, (captured_id, _)) in self.current_block.captures.iter().enumerate() {
            if captured_id == id {
                return Some(index);
            }
        }

        if self.current_outer_blocks.is_empty() {
            return None;
        }
        let index = self.current_outer_blocks.len() - 1;
        let source = Self::try_capture_impl(id, &mut self.current_outer_blocks, index)?;

        let captured_index = self.current_block.captures.len();
        self.current_block.captures.push((id.into(), source));
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
