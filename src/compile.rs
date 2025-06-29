use std::{
    collections::{HashMap, HashSet},
    mem::{replace, take},
};

use thiserror::Error;

use crate::{
    code::{
        Block, CaptureSource, Expr, Instr, InstrIndex, Literal, Stmt, ValueIndex, instr::Intrinsic,
    },
    intern::StringPool,
};

pub struct Compile {
    main: Vec<Instr>,
    packages: HashMap<String, Package>,

    current_package_name: String,
    current_package: Package,
    current_block: CompileBlock,
    current_outer_blocks: Vec<CompileBlock>,

    intrinsics: HashMap<String, Intrinsic>,
    pub string_pool: StringPool,
}

#[derive(Default)]
struct CompileBlock {
    instrs: Vec<Instr>,
    promoted_indexes: HashSet<ValueIndex>,
    expr_index: ValueIndex,
    max_index: ValueIndex,
    scopes: Vec<HashMap<String, ValueIndex>>,
    loop_jump_targets: Vec<InstrIndex>,
    // to be translated into Capture instructions
    captures: Vec<(String, CaptureSource)>,
}

#[derive(Default)]
struct Package {
    exports: HashMap<String, ValueIndex>,
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
}

impl Compile {
    pub fn input(&mut self, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        for stmt in stmts {
            self.input_stmt(stmt)?
        }

        assert!(self.current_outer_blocks.is_empty());
        self.main.extend(take(&mut self.current_block).instrs);

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

    fn input_stmt(&mut self, stmt: Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Package(name) => {
                let replaced = replace(&mut self.current_package_name, name);
                if !replaced.is_empty() {
                    return Err(CompileError::MultiplePackageStmt(replaced));
                }
            }
            Stmt::Export(id) => todo!(),

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
                self.input_expr(expr)?;
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
                let Some(&native_fn) = self.intrinsics.get(&intrinsic.id) else {
                    return Err(CompileError::UnknownIntrinsic(intrinsic.id));
                };
                let expr_index = self.current_block.expr_index;
                self.current_block.expr_index += intrinsic.dst_ids.len();
                // first evaluate arguments without destination ids in scope
                for expr in intrinsic.args {
                    self.input_expr(expr)?;
                    self.current_block.expr_index += 1
                }
                for (index, id) in intrinsic.dst_ids.into_iter().enumerate() {
                    self.bind(id, expr_index + index)
                }
                self.add(Instr::Intrinsic(
                    native_fn,
                    (expr_index..self.current_block.expr_index).collect(),
                ));
                self.current_block.expr_index = expr_index
            }

            Stmt::Return(expr) => {
                self.input_expr(expr)?;
                self.add(Instr::Return(self.current_block.expr_index))
            }
            Stmt::Wait(expr) => {
                self.input_expr(expr)?;
                self.add(Instr::Wait(self.current_block.expr_index))
            }
            Stmt::Notify(expr) => {
                self.input_expr(expr)?;
                self.add(Instr::Notify(self.current_block.expr_index))
            }
            Stmt::Spawn(expr) => {
                self.input_expr(expr)?;
                self.add(Instr::Spawn(self.current_block.expr_index))
            }
            Stmt::Bind(id, expr) => {
                self.input_expr(expr)?;
                self.bind(id, self.current_block.expr_index);
                self.current_block.expr_index += 1;
            }

            Stmt::Expr(expr) => self.input_expr(expr)?,
            // memo for compiling mutation
            // three cases. mutating owning plain value: just Copy. mutating promoted value:
            // SetPromoted. mutating captured value: SetCaptured
        }
        Ok(())
    }

    fn input_expr(&mut self, expr: Expr) -> Result<(), CompileError> {
        let expr_index = self.current_block.expr_index;
        self.current_block.max_index = self.current_block.max_index.max(expr_index + 1);
        match expr {
            Expr::Import(_, _) => todo!(),

            Expr::Literal(Literal::Func(func)) => {
                self.current_outer_blocks
                    .push(take(&mut self.current_block));
                let num_param = func.params.len();
                for id in func.params {
                    self.bind(id, self.current_block.expr_index);
                    self.current_block.expr_index += 1
                }
                self.input_expr(*func.body)?;

                let func_block = replace(
                    &mut self.current_block,
                    self.current_outer_blocks.pop().unwrap(),
                );
                let block = Block {
                    name: "TODO".into(),
                    num_param,
                    num_value: func_block.max_index,
                    instrs: func_block.instrs,
                };
                self.add(Instr::MakeClosure(expr_index, block.into()));
                for (_, capture_source) in func_block.captures {
                    if let &CaptureSource::Owning(index) = &capture_source
                        && self.current_block.promoted_indexes.insert(index)
                    {
                        self.add(Instr::Promote(index))
                    }
                    self.add(Instr::Capture(expr_index, capture_source))
                }
            }
            Expr::Call(closure, args) => {
                self.input_expr(*closure)?;
                for arg in args {
                    self.current_block.expr_index += 1;
                    self.input_expr(arg)?
                }
                self.add(Instr::Call(
                    expr_index,
                    expr_index,
                    (expr_index + 1..=self.current_block.expr_index).collect(),
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
                    self.input_stmt(stmt)?
                }
                self.input_expr(*expr)?;
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
            return Some(CaptureSource::Owning(value_index));
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
