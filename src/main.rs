mod asset;
mod code;
mod compile;
mod eval;
mod oracle;
mod parse;
mod sched;
mod space;
mod task;
mod typing;
mod worker;

use std::error::Error;

use crate::{
    asset::Asset, compile::Compile, eval::intrinsics, parse::Source, typing::TypeRegistry,
};

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

    let mut registry = TypeRegistry::new();
    let mut compile = Compile::new();

    let source = SOURCE.parse::<Source>()?;
    println!("{source:?}");

    let mut asset = Asset::new();
    registry.preload(&mut asset);
    intrinsics::preload(&mut asset);
    compile.input(source.stmts, &mut asset)?;
    let prog = compile.finish(&mut asset);

    for block_id in 0..=prog.block_id {
        println!("{}", asset.display_block(block_id))
    }

    worker::run(prog, registry, &asset);
    Ok(())
}
