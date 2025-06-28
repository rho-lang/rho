use crate::{
    eval::{ExecuteError, Value},
    intern::StringId,
    oracle::Oracle,
    space::Space,
    typing::RecordLayout,
};

#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),

    Loop(Expr),
    Break,
    Continue,
    Return(Expr),

    // concurrency primitives
    Wait(Expr),   // wait on a Future to notify
    Notify(Expr), // notify all tasks `Wait`ing on a Future
    Spawn(Expr),

    Assign(String, Expr),
    Package(String),
    Export(String),
    Intrinsic(syntax::Intrinsic),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Import(String, String),
    Var(String),
    Compound(Vec<Stmt>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Match(Box<syntax::Match>),

    Future, // the synchronization object
}

#[derive(Debug)]
pub enum Literal {
    Unit,
    String(String),
    Func(Func),
}

pub mod syntax {
    use super::*;

    // if `scrutinee` (e.g., `x`) matches `pattern` (e.g., `True`), evaluate
    // `and_then`, otherwise evaluate `or_else`
    #[derive(Debug)]
    pub struct Match {
        pub scrutinee: Expr, // the field name is borrowed from Rust reference on pattern matching
        pub pattern: Expr,
        pub and_then: Expr,
        pub or_else: Expr,
    }

    #[derive(Debug)]
    pub struct Intrinsic {
        pub id: String,
        pub dst_ids: Vec<String>,
        pub args: Vec<Expr>,
    }
}

#[derive(Debug)]
pub struct Func {
    // filled by parsing
    pub params: Vec<String>,
    pub body: Box<Expr>,
}

pub type InstrIndex = usize;
pub type ValueIndex = usize;

#[derive(Debug)]
pub enum Instr {
    MakeUnit(ValueIndex),
    MakeString(ValueIndex, String),
    MakeClosure(ValueIndex, Box<Block>, Vec<ValueIndex>), // destination, block, captured values

    MakeRecordType(ValueIndex, RecordLayout),
    MakeRecord(ValueIndex, ValueIndex, Vec<(StringId, ValueIndex)>),

    MakeFuture(ValueIndex),

    // here we have a dedicated instruction for copying captured value. firstly, it
    // simplifies the implementation of compilation. secondly, captured values are
    // not on stack at the first place; they reside as closure's property. a lazy
    // copying potentially improve performance especially when a lot of values are
    // captured for accessing in different cases
    Copy(ValueIndex, ValueIndex),
    CopyCaptured(ValueIndex, usize),

    GetAttr(ValueIndex, ValueIndex, StringId), // #0 <- #1.#2
    SetAttr(ValueIndex, StringId, ValueIndex), // #0.#1 <- #2

    Call(ValueIndex, ValueIndex, Vec<ValueIndex>), // destination, closure, arguments
    Jump(InstrIndex, Option<instr::Match>),
    Return(ValueIndex),

    Spawn(ValueIndex),
    Wait(ValueIndex),
    Notify(ValueIndex),

    Intrinsic(instr::Intrinsic, Vec<ValueIndex>),
}

pub mod instr {
    use super::*;

    // currently not seen any intrinsic that need to be `unsafe`
    pub type Intrinsic =
        fn(&mut [Value], &[ValueIndex], &mut Space, &mut Oracle) -> Result<(), ExecuteError>;

    #[derive(Debug)]
    pub struct Match {
        pub scrutinee: ValueIndex,
        pub pattern: ValueIndex,
    }
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub num_captured: usize,
    pub num_param: usize,
    pub num_value: usize,
    pub instrs: Vec<Instr>,
}
