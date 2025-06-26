mod code;
mod eval;
mod sched;
mod space;
mod task;
mod worker;

use std::{error::Error, sync::Arc};

use crate::{
    code::{Block, Instr},
    eval::{Closure, Eval, intrinsics},
    sched::Sched,
    space::Space,
    task::Task,
};

fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt::init();

    let simple = Block {
        name: "simple".into(),
        instrs: vec![
            Instr::LoadString(0, "hello".into()),
            Instr::Intrinsic(intrinsics::trace, vec![0]),
            Instr::LoadUnit(0),
            Instr::Return(0),
        ],
        num_param: 0,
        num_captured: 0,
        num_value: 1,
    };
    let prog = Closure::main(
        vec![
            Instr::LoadClosure(0, Arc::new(simple), vec![]),
            Instr::Call(0, 0, vec![]),
            Instr::Return(0),
        ],
        1,
    );

    worker::run(&prog);
    unsafe { prog.drop_main() }
    Ok(())
}
