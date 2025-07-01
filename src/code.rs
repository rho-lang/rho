use crate::{
    asset::{BlockId, StringId},
    eval::{ExecuteError, Value},
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
    Wait(Expr),   // wait on an Event to notify
    Notify(Expr), // notify all tasks `Wait`ing on an Event
    Spawn(Expr),

    Bind(String, Expr),
    Mut(String, Expr),
    MutAttr(Expr, String, Expr),

    Package(String),
    Export(String),

    Intrinsic(syntax::Intrinsic),
}

#[derive(Debug)]
pub enum Expr {
    Literal(Literal),
    Import(String, String),
    Var(String),

    Type(Vec<String>),
    Op(String, Vec<Expr>),
    GetAttr(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    Match(Box<syntax::Match>),
    Record(Box<Expr>, Vec<(String, Expr)>),
    Compound(Vec<Stmt>, Box<Expr>),
    Func(Func),
}

#[derive(Debug)]
pub enum Literal {
    Unit,
    Event, // the synchronization object
    String(String),
    I32(i32),
}

pub mod syntax {
    use super::*;

    // if `scrutinee` (e.g., `x`) matches `pattern` (e.g., `True`), evaluate
    // `and_then`, otherwise evaluate `or_else`
    #[derive(Debug)]
    pub struct Match {
        pub scrutinee: Expr, // the field name is borrowed from Rust reference on pattern matching
        pub cases: Vec<(Expr, Expr)>, // (pattern, value)
    }

    #[derive(Debug)]
    pub struct Intrinsic {
        pub id: String,
        pub bindings: Vec<String>,
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
    MakeI32(ValueIndex, i32),

    MakeClosure(ValueIndex, BlockId), // dst, block, captured
    // promote the value into a cell for capturing. promote is a separated operation
    // to capture for 1. multiple concurrent capture 2. recursive capture
    Promote(ValueIndex),
    Capture(ValueIndex, CaptureSource),
    // inner access
    GetCaptured(ValueIndex, usize),
    SetCaptured(usize, ValueIndex),
    // outer access
    Demote(ValueIndex),
    SetPromoted(ValueIndex, ValueIndex),

    MakeRecordType(ValueIndex, RecordLayout),
    MakeRecord(ValueIndex, ValueIndex, Vec<(StringId, ValueIndex)>), // dst, type_id, attrs

    MakeEvent(ValueIndex),

    Copy(ValueIndex, ValueIndex),
    Op2(ValueIndex, Op2, ValueIndex, ValueIndex),

    GetAttr(ValueIndex, ValueIndex, StringId), // #0 <- #1.#2
    SetAttr(ValueIndex, StringId, ValueIndex), // #0.#1 <- #2

    Jump(InstrIndex, Option<instr::Match>),
    Call(ValueIndex, ValueIndex, Vec<ValueIndex>), // dst, closure, args
    Return(ValueIndex),

    Spawn(ValueIndex),
    Wait(ValueIndex),
    Notify(ValueIndex),

    Intrinsic(instr::Intrinsic, Vec<ValueIndex>),
}

#[derive(Debug)]
pub enum CaptureSource {
    Original(ValueIndex),
    Transitive(usize),
}

#[derive(Debug)]
pub enum Op2 {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // And,
    // Or,
    // Xor,
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

pub mod instr {
    use crate::worker::WorkerContext;

    use super::*;

    // currently not seen any intrinsic that need to be `unsafe`
    pub type Intrinsic =
        fn(&mut [Value], &[ValueIndex], &mut WorkerContext) -> Result<(), ExecuteError>;

    #[derive(Debug)]
    pub struct Match {
        pub scrutinee: ValueIndex,
        pub pattern: ValueIndex,
    }
}

#[derive(Debug)]
pub struct Block {
    pub name: String,
    pub num_param: usize,
    pub num_value: usize,
    pub instrs: Vec<Instr>,
}
