use std::collections::HashMap;

use crate::code::Block;

pub type StringId = u32;
pub type BlockId = u32;

#[derive(Debug, Default)]
pub struct Asset {
    string_ids: HashMap<String, StringId>,
    strings: Vec<String>,

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
}
