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

use crate::{code::instr::Intrinsic, compile::Compile, eval::intrinsics, parse::Source};

const SOURCE: &str = r#"
let fut1 = future
let simple = func() {
    intrinsic trace ("[simple] start")
    let fut2 = future
    intrinsic notify_after (fut2, "10ms")
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
    let mut compile = Compile::new();
    compile.intrinsics = [
        ("trace", intrinsics::trace as Intrinsic),
        ("notify_after", intrinsics::notify_after),
        //
    ]
    .map(|(s, f)| (s.into(), f))
    .into();
    compile.input(source.stmts)?;
    let prog = compile.finish();
    worker::run(prog);
    unsafe { prog.drop_main() }
    Ok(())
}
