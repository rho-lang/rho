use std::{
    collections::HashMap,
    mem::{replace, take},
};

use thiserror::Error;

use crate::code::{Instr, Stmt, ValueIndex};

pub struct Compile {
    main: Vec<Instr>,
    packages: HashMap<String, Package>,

    current_package_name: String,
    current_package: Package,
    current_blocks: Vec<CompileBlock>,
}

struct CompileBlock {
    scopes: Vec<HashMap<String, ValueIndex>>,
    instrs: Vec<Instr>,
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
}

impl Compile {
    pub fn input(&mut self, stmts: Vec<Stmt>) -> Result<(), CompileError> {
        for stmt in stmts {
            //
        }

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
                if replaced.is_empty() {
                    Ok(())
                } else {
                    Err(CompileError::MultiplePackageStmt(replaced))
                }
            }
            Stmt::Expr(expr) => todo!(),
            Stmt::Loop(expr) => todo!(),
            Stmt::Break => todo!(),
            Stmt::Continue => todo!(),
            Stmt::Return(expr) => todo!(),
            Stmt::Wait(expr) => todo!(),
            Stmt::Notify(expr) => todo!(),
            Stmt::Spawn(expr) => todo!(),
            Stmt::Assign(_, expr) => todo!(),
            Stmt::Export(_) => todo!(),
            Stmt::Intrinsic(intrinsic) => todo!(),
        }
    }
}
