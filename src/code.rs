pub enum Stmt {
    Expr(Expr),
    Assign(String, Expr),
    While(Expr, Expr),
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
    String(String),
    Func(Func),
}

// if `actual` (e.g., `x`) matches `expected` (e.g., `True`), evaluate
// `and_then`, otherwise evaluate `or_else`
pub struct Match {
    pub actual: Expr,
    pub expected: Expr,
    pub and_then: Expr,
    pub or_else: Expr,
}

pub struct Func {
    pub params: Vec<String>,
    pub body: Box<Expr>,
}
