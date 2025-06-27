use std::sync::Arc;

use crate::{
    eval::{ExecuteError, TypeError, Value},
    oracle::Oracle,
    space::Space,
};

pub enum Stmt {
    Expr(Expr),
    Assign(String, Expr),
    Loop(Expr),
    Break,
    Continue,
    Return(Expr),
}

pub enum Expr {
    Literal(Literal),
    Var(String),
    Compound(Vec<Stmt>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Match(Box<Match>),
    // concurrency primitives
    Spawn(Func),
    Future,            // the synchronization object
    Wait(Box<Expr>),   // wait on a Future to notify
    Notify(Box<Expr>), // notify all tasks `Wait`ing on a Future
}

pub enum Literal {
    Unit,
    String(String),
    Func(Func),
}

// if `scrutinee` (e.g., `x`) matches `pattern` (e.g., `True`), evaluate
// `and_then`, otherwise evaluate `or_else`
pub struct Match {
    pub scrutinee: Expr, // the field name is borrowed from Rust reference on pattern matching
    pub pattern: Expr,
    pub and_then: Expr,
    pub or_else: Expr,
}

pub struct Func {
    pub params: Vec<String>,
    pub body: Box<Expr>,
}

pub type InstrIndex = usize;
pub type ValueIndex = usize;

#[derive(Debug)]
pub enum Instr {
    LoadUnit(ValueIndex),
    LoadString(ValueIndex, String),
    LoadClosure(ValueIndex, Arc<Block>, Vec<ValueIndex>), // destination, block, captured values

    Copy(ValueIndex, ValueIndex),
    Call(ValueIndex, ValueIndex, Vec<ValueIndex>), // destination, closure, arguments
    Jump(InstrIndex, Option<JumpCond>),
    Return(ValueIndex),

    Spawn(ValueIndex),
    LoadFuture(ValueIndex),
    Wait(ValueIndex),
    Notify(ValueIndex),

    Intrinsic(Intrinsic, Vec<ValueIndex>),
}

// currently not seen any intrinsic that need to be `unsafe`
pub type Intrinsic =
    fn(&mut [Value], &[ValueIndex], &mut Space, &mut Oracle) -> Result<(), ExecuteError>;

#[derive(Debug)]
pub struct JumpCond {
    pub scrutinee: ValueIndex,
    pub pattern: ValueIndex,
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub num_captured: usize,
    pub num_param: usize,
    pub num_value: usize,
    pub instrs: Vec<Instr>,
}
