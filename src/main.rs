mod code;
mod eval;
mod sched;
mod space;
mod task;
mod worker;

use std::{error::Error, sync::Arc};

use crate::{
    code::{Block, Instr},
    eval::{Closure, Eval},
    space::Space,
};

fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt::init();

    let simple = Block {
        name: "simple".into(),
        instrs: vec![Instr::LoadUnit(0), Instr::Return(0)],
        num_param: 0,
        num_captured: 0,
        num_value: 1,
    };
    let call_simple = Closure::main(
        vec![
            Instr::LoadClosure(0, Arc::new(simple), vec![]),
            Instr::Call(0, 0, vec![]),
            Instr::Return(0),
        ],
        1,
    );

    let mut eval = Eval::new(&call_simple)?;
    eval.execute(&mut Space::new(4 << 10))?;
    unsafe { call_simple.drop_main() }
    Ok(())
}
