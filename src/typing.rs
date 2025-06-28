use std::collections::HashMap;

use crate::intern::StringId;

#[derive(Debug, Clone)]
pub struct RecordLayout(pub Vec<StringId>);

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeId(pub u32);

impl TypeId {
    pub const VACANT: Self = Self(0);
    pub const TYPE_ID: Self = Self(1);
    pub const UNIT: Self = Self(2);
    pub const STRING: Self = Self(3);
    pub const CLOSURE: Self = Self(4);
    pub const FUTURE: Self = Self(5);

    pub const RESERVED_MAX: Self = Self(100);
}

pub struct TypeRegistry {
    type_id: TypeId,
    record_layouts: HashMap<TypeId, RecordLayout>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            type_id: TypeId::RESERVED_MAX,
            // do we need to insert layouts for zero sized record types?
            record_layouts: [
                (TypeId::UNIT, RecordLayout(vec![])),
                //
            ]
            .into(),
        }
    }

    pub fn add_record_type(&mut self, record_layout: RecordLayout) -> TypeId {
        self.type_id.0 += 1;
        self.record_layouts.insert(self.type_id, record_layout);
        self.type_id
    }

    pub fn get_record_layout(&self, type_id: TypeId) -> Option<&RecordLayout> {
        self.record_layouts.get(&type_id)
    }
}
