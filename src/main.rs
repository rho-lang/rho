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

use std::{
    collections::HashSet,
    env::args,
    error::Error,
    fs::read_to_string,
    path::{Path, PathBuf},
};

use walkdir::WalkDir;

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
    let prog = compile_sources(Path::new(&source).canonicalize()?, &mut asset)?;

    for block_id in 0..=prog.block_id {
        println!("{}", asset.display_block(block_id))
    }

    worker::run(prog, registry, &asset);
    Ok(())
}

fn compile_sources(path: impl AsRef<Path>, asset: &mut Asset) -> Result<Closure, Box<dyn Error>> {
    fn walk(
        path: PathBuf,
        compile: &mut Compile,
        asset: &mut Asset,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<(), Box<dyn Error>> {
        if !visited.insert(path.clone()) {
            return Ok(());
        }
        tracing::debug!(path = %path.display(), "compile");
        let mut source_string = read_to_string(&path)?;
        source_string.push('\n'); // assumed by Source::from_str
        let source = source_string.parse::<Source>()?;
        for input in source.inputs {
            let input = path
                .parent()
                .expect("source file has parent")
                .join(input)
                .canonicalize()?;
            if input.is_file() {
                walk(input, compile, asset, visited)?
            } else if input.extension().is_none() && input.with_extension("rho").is_file() {
                walk(input.with_extension("rho"), compile, asset, visited)?
            } else if input.is_dir() {
                for entry in WalkDir::new(input) {
                    let entry = entry?;
                    let path = entry.path();
                    if path.is_file() && path.extension() == Some("rho".as_ref()) {
                        walk(path.into(), compile, asset, visited)?
                    }
                }
            } else {
                return Err(format!("invalid input {}", input.display()).into());
            }
        }
        compile.input(source.stmts, asset)?;
        Ok(())
    }

    let mut compile = Compile::new();
    walk(
        path.as_ref().into(),
        &mut compile,
        asset,
        &mut Default::default(),
    )?;
    Ok(compile.finish(asset))
}
