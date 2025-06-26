mod code;
mod eval;
mod sched;
mod space;
mod task;
mod worker;

use std::{error::Error, sync::Arc};

use crate::{
    code::{Block, Instr},
    eval::{Closure, intrinsics},
};

fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt::init();

    let simple = Block {
        name: "simple".into(),
        instrs: vec![
            Instr::LoadString(1, "[simple] start".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::Notify(0),
            Instr::LoadString(1, "[simple] notified".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::LoadUnit(0),
            Instr::Return(0),
        ],
        num_param: 0,
        num_captured: 1,
        num_value: 2,
    };
    let prog = Closure::main(
        vec![
            Instr::LoadFuture(0),
            Instr::LoadClosure(1, Arc::new(simple), vec![0]),
            Instr::Spawn(1),
            Instr::LoadString(1, "[main] spawned".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::Wait(0),
            Instr::LoadString(1, "[main] wait finish".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::LoadUnit(0),
            Instr::Return(0),
        ],
        2,
    );

    worker::run(&prog);
    unsafe { prog.drop_main() }
    Ok(())
}
