use std::collections::HashMap;

pub type StringId = u32;

#[derive(Debug, Default)]
pub struct Asset {
    string_ids: HashMap<String, StringId>,
    strings: Vec<String>,
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
}
