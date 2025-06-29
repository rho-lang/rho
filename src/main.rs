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

use std::{env::args, error::Error, fs::read_to_string, path::Path};

use crate::{
    asset::Asset,
    compile::Compile,
    eval::{Closure, intrinsics},
    parse::Source,
    typing::TypeRegistry,
};

fn main() -> Result<(), Box<dyn Error>> {
    tracing_subscriber::fmt::init();

    let mut registry = TypeRegistry::new();
    let mut asset = Asset::new();
    registry.preload(&mut asset);
    intrinsics::preload(&mut asset);

    let Some(source) = args().nth(1) else {
        return Err("source is not specified".into());
    };
    let prog = compile_sources(Path::new(&source), &mut asset)?;

    for block_id in 0..=prog.block_id {
        println!("{}", asset.display_block(block_id))
    }

    worker::run(prog, registry, &asset);
    Ok(())
}

fn compile_sources(path: &Path, asset: &mut Asset) -> Result<Closure, Box<dyn Error>> {
    fn walk(path: &Path, compile: &mut Compile, asset: &mut Asset) -> Result<(), Box<dyn Error>> {
        let source = read_to_string(path)?.parse::<Source>()?;
        for input in source.inputs {
            let input = Path::new(&input);
            if input.is_file() {
                walk(input, compile, asset)?
            } else {
                todo!()
            }
        }
        compile.input(source.stmts, asset)?;
        Ok(())
    }

    let mut compile = Compile::new();
    walk(path, &mut compile, asset)?;
    Ok(compile.finish(asset))
}
