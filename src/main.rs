mod asset;
mod code;
mod compile;
mod eval;
mod parse;
mod sched;
mod space;
mod task;
mod typing;
mod worker;

use std::{
    collections::HashSet,
    env::{self, args},
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
    let prog = compile_sources(Path::new(&source), &mut asset)?;

    if let Ok(value) = env::var("RHO_DUMP_BLOCKS")
        && value != "0"
    {
        for block_id in 0..=prog.block_id {
            println!("{}", asset.display_block(block_id))
        }
        return Ok(());
    }

    worker::run(prog, registry, asset.into());
    Ok(())
}

fn compile_sources(path: impl AsRef<Path>, asset: &mut Asset) -> Result<Closure, Box<dyn Error>> {
    fn walk(
        path: PathBuf,
        compile: &mut Compile,
        asset: &mut Asset,
        compiled: &mut HashSet<PathBuf>,
        visited: &mut HashSet<PathBuf>,
    ) -> Result<(), Box<dyn Error>> {
        if compiled.contains(&path) {
            return Ok(());
        }
        if !visited.insert(path.clone()) {
            return Err(format!("circular dependency detected: {}", path.display()).into());
        }

        tracing::debug!(path = %path.display(), "compile");
        let mut source_string = read_to_string(&path)?;
        source_string.push('\n'); // assumed by Source::from_str
        let source = source_string.parse::<Source>()?;
        for input in source.inputs {
            let input = path.parent().expect("source file has parent").join(input);
            if input.is_file() {
                walk(input.canonicalize()?, compile, asset, compiled, visited)?
            } else if input.extension().is_none() && input.with_extension("rho").is_file() {
                walk(
                    input.with_extension("rho").canonicalize()?,
                    compile,
                    asset,
                    compiled,
                    visited,
                )?
            } else if input.is_dir() {
                for entry in WalkDir::new(input) {
                    let entry = entry?;
                    let path = entry.path().canonicalize()?;
                    if path.is_file() && path.extension() == Some("rho".as_ref()) {
                        walk(path, compile, asset, compiled, visited)?
                    }
                }
            } else {
                return Err(format!("invalid input {}", input.display()).into());
            }
        }

        compile.input(source.stmts, asset)?;
        compiled.insert(path.clone());
        Ok(())
    }

    let mut compile = Compile::new();
    walk(
        path.as_ref().canonicalize()?,
        &mut compile,
        asset,
        &mut Default::default(),
        &mut Default::default(),
    )?;
    Ok(compile.finish(asset))
}
