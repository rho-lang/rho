mod code;
mod compile;
mod eval;
mod intern;
mod oracle;
mod parse;
mod sched;
mod space;
mod task;
mod typing;
mod worker;

use std::error::Error;

use crate::{
    code::{Block, CaptureSource, Instr},
    eval::{Closure, intrinsics},
    intern::StringId,
    parse::Source,
    typing::RecordLayout,
};

const SOURCE: &str = r#"
let fut1 = future
let simple = func() {
    intrinsic trace ("[simple] start")
    let fut2 = future
    intrinsic notify_after (fut2, "1s")
    wait fut2
    notify fut1
    intrinsic trace ("[simple] notified")
}
spawn simple
intrinsic trace ("[main] spawned")
wait fut1
intrinsic trace ("[main] wait finish")
"#;

fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt::init();

    let source = SOURCE.parse::<Source>()?;
    println!("{source:?}");

    let simple = Block {
        name: "simple".into(),
        instrs: vec![
            Instr::MakeString(0, "[simple] start".into()),
            Instr::Intrinsic(intrinsics::trace, vec![0]),
            Instr::MakeFuture(0),
            Instr::MakeString(1, "1s".into()),
            Instr::Intrinsic(intrinsics::notify_after, vec![0, 1]),
            Instr::Wait(0),
            Instr::GetCaptured(0, 0),
            Instr::Notify(0),
            Instr::MakeString(0, "[simple] notified".into()),
            Instr::Intrinsic(intrinsics::trace, vec![0]),
            Instr::MakeUnit(0),
            Instr::Return(0),
        ],
        num_param: 0,
        num_value: 2,
    };
    let prog = Closure::main(
        vec![
            Instr::MakeFuture(0),
            Instr::MakeClosure(1, simple.into()),
            Instr::Promote(0),
            Instr::Capture(1, CaptureSource::Value(0)),
            Instr::Spawn(1),
            Instr::MakeString(1, "[main] spawned".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::Demote(0),
            Instr::Wait(0),
            Instr::MakeString(0, "[main] wait finish".into()),
            Instr::Intrinsic(intrinsics::trace, vec![0]),
            Instr::MakeUnit(0),
            Instr::Return(0),
        ],
        2,
    );

    worker::run(prog);
    unsafe { prog.drop_main() }
    Ok(())
}
