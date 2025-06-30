use std::collections::HashMap;

use crate::asset::{Asset, StringId};

#[derive(Debug, Clone)]
pub struct RecordLayout(pub Vec<StringId>);

// a newtype for dual purposes. first it gives us chance to define
// type-associated constants for essential types and customize debug format.
// second, it is the presentation of "type" type that appears in the virtual
// machine. for example
// let MyType = type (foo, bar, baz)
// creates a TypeId Value { type_id: TypeId::TYPE_ID, data: TypeId(123) } on the
// call frame, while a RecordLayout is registered for TypeId(123) in 
// TypeRegistry
// it is possible (or even preferred) to store the type metadata directly on the
// heap (i.e. Space) and use its SpaceAddr as id. the rationale for a dedicated
// storage (i.e. TypeRegistry) is to compress TypeId domain to 24 bits, expected
// by `Value`'s layout (see its comments for reference). if storing on the heap,
// the SpaceAddr will be scattered in a 40-bit address space
// the major downside of current design may be it impedes garbage collection of
// unused type definitions. i don't think it would be a common practice to
// define tremendous (unused) types. and if (really) necessary, "type marking"
// can be integrated into garbage collection to identify types without any
// reachable instantiated records and/or TypeId
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeId(pub u32);

impl TypeId {
    pub const VACANT: Self = Self(0);

    // core native types
    pub const TYPE_ID: Self = Self(1);
    pub const CLOSURE: Self = Self(2);
    // basically just Value, but dedicated for closure capturing
    // following Python's terminology
    pub const CELL: Self = Self(3);
    pub const FUTURE: Self = Self(4);

    // core record types
    pub const UNIT: Self = Self(5);

    // essential (native) types
    pub const STRING: Self = Self(6);

    pub const RESERVED_MAX: Self = Self(100);
}

#[derive(Debug, Clone)]
pub struct TypeRegistry {
    type_id: TypeId,
    record_layouts: HashMap<TypeId, RecordLayout>,
}

impl Default for TypeRegistry {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            type_id: TypeId::RESERVED_MAX,
            record_layouts: Default::default(),
        }
    }

    pub fn preload(&mut self, _asset: &mut Asset) {
        self.record_layouts = [
            // do we need to insert layouts for zero sized record types?
            (TypeId::UNIT, RecordLayout(vec![])),
        ]
        .into()
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
