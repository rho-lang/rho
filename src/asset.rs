use std::{
    collections::HashMap,
    fmt::{self, Display, Formatter},
    ptr::fn_addr_eq,
};

use crate::{
    code::{Block, instr::Intrinsic},
    typing::RecordLayout,
};

pub type StringId = u32;
pub type BlockId = u32;

#[derive(Debug, Default)]
pub struct Asset {
    string_ids: HashMap<String, StringId>,
    strings: Vec<String>,

    pub intrinsics: HashMap<String, Intrinsic>,

    blocks: Vec<Block>,
}

impl Asset {
    #[allow(unused)]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, s: String) -> StringId {
        if let Some(&id) = self.string_ids.get(&s) {
            id
        } else {
            let string_id = self.strings.len() as _;
            self.strings.push(s.clone());
            self.string_ids.insert(s, string_id);
            string_id
        }
    }

    pub fn get_string(&self, id: StringId) -> &str {
        &self.strings[id as usize]
    }

    pub fn add_block(&mut self, block: Block) -> BlockId {
        let id = self.blocks.len() as _;
        self.blocks.push(block);
        id
    }

    pub fn get_block(&self, id: BlockId) -> &Block {
        &self.blocks[id as usize]
    }

    fn fmt_block(&self, id: BlockId, f: &mut Formatter<'_>) -> fmt::Result {
        let block = self.get_block(id);
        writeln!(f, "block#{id} {}/{}", block.name, block.num_param)?;
        write!(f, "    #value = {}", block.num_value)?;
        for (i, instr) in block.instrs.iter().enumerate() {
            writeln!(f)?;
            write!(f, "  {i:4} ")?;
            use crate::code::Instr::*;
            match instr {
                MakeClosure(dst, block_id) => write!(f, "MakeClosure({dst}, block#{block_id})")?,
                MakeRecordType(dst, RecordLayout(attrs)) => {
                    write!(f, "MakeRecordType({dst}, ...)")?;
                    for &attr in attrs {
                        writeln!(f)?;
                        write!(f, "       {}(attr)", self.get_string(attr))?
                    }
                }
                MakeRecord(dst, type_id, attrs) => {
                    write!(f, "MakeRecord({dst}, {type_id}, ...)")?;
                    for &(attr, value_index) in attrs {
                        writeln!(f)?;
                        write!(f, "       {}(attr): <{value_index}>", self.get_string(attr))?
                    }
                }
                Intrinsic(native_fn, indexes) => {
                    let id = self
                        .intrinsics
                        .iter()
                        .find(|&(_, &f)| fn_addr_eq(f, *native_fn))
                        .map(|(id, _)| &**id)
                        .unwrap_or("??");
                    write!(f, "Intrinsic({id}@{native_fn:?}, {indexes:?})")?
                }
                _ => write!(f, "{instr:?}")?,
            }
        }
        Ok(())
    }

    pub fn display_block(&self, id: BlockId) -> DisplayBlock<'_> {
        DisplayBlock(self, id)
    }
}

pub struct DisplayBlock<'a>(&'a Asset, BlockId);

impl Display for DisplayBlock<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.0.fmt_block(self.1, f)
    }
}
