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
    code::{Block, Instr},
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

    const STRING_FUT1: StringId = 123;
    let simple = Block {
        name: "simple".into(),
        instrs: vec![
            Instr::GetAttr(0, 0, STRING_FUT1),
            Instr::MakeString(1, "[simple] start".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::MakeFuture(1),
            Instr::MakeString(2, "1s".into()),
            Instr::Intrinsic(intrinsics::notify_after, vec![1, 2]),
            Instr::Wait(1),
            Instr::Notify(0),
            Instr::MakeString(1, "[simple] notified".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::MakeUnit(0),
            Instr::Return(0),
        ],
        num_param: 0,
        num_captured: 1,
        num_value: 3,
    };
    let prog = Closure::main(
        vec![
            Instr::MakeFuture(0),
            Instr::MakeRecordType(1, RecordLayout(vec![STRING_FUT1])),
            Instr::MakeRecord(1, 1, vec![(STRING_FUT1, 0)]),
            Instr::MakeClosure(1, simple.into(), Some(1)),
            Instr::Spawn(1),
            Instr::MakeString(1, "[main] spawned".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::Wait(0),
            Instr::MakeString(1, "[main] wait finish".into()),
            Instr::Intrinsic(intrinsics::trace, vec![1]),
            Instr::MakeUnit(0),
            Instr::Return(0),
        ],
        2,
    );

    worker::run(prog);
    unsafe { prog.drop_main() }
    Ok(())
}
