use std::collections::HashMap;

pub type StringId = u32;

#[derive(Debug, Default)]
pub struct StringPool {
    map: HashMap<String, StringId>,
    strings: Vec<String>,
}

impl StringPool {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, s: String) -> StringId {
        if let Some(&id) = self.map.get(&s) {
            id
        } else {
            let string_id = self.strings.len() as _;
            self.strings.push(s.clone());
            self.map.insert(s, string_id);
            string_id
        }
    }

    pub fn get(&self, id: StringId) -> &str {
        &self.strings[id as usize]
    }
}
