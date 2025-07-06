use std::{
    alloc::Layout,
    ptr::{copy, copy_nonoverlapping},
};

use thiserror::Error;

use crate::{
    asset::Asset,
    code::{ValueIndex, instr::Intrinsic},
    eval::{Eval, ExecuteError, TaskView, TypeError, Value, ValuesView},
    space::{OutOfSpace, Space, SpaceAddr},
    typing::{RecordLayout, TypeId, TypeRegistry},
};

pub fn preload(asset: &mut Asset) {
    START.with(|_| {}); // initialize the thread-local variable
    asset.intrinsics = [
        ("list_cap", list_cap as Intrinsic),
        ("list_copy_within", list_copy_within),
        ("list_copy", list_copy),
        ("list_len", list_len),
        ("list_load", list_load),
        ("list_new", list_new),
        ("list_set_len", list_set_len),
        ("list_store", list_store),
        ("park", park),
        ("task_new", task_new),
        ("time_since_start", time_since_start),
        ("trace", trace),
        ("type_object", type_object),
    ]
    .map(|(s, f)| (s.into(), f))
    .into()
}

#[derive(Error, Debug)]
pub enum Error {
    #[error("{0}")]
    ParseDuration(#[from] humantime::DurationError),
}

unsafe fn task_new(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    asset: &Asset,
) -> Result<(), ExecuteError> {
    let closure =
        unsafe { values.load(indexes[1], &context.space) }.load_closure(&context.space)?;
    let task_value = Value::alloc_task(&mut context.space)?;
    unsafe { TaskView(task_value.addr()).push_frame(closure, &[], &mut context.space, asset)? };
    unsafe { values.store(indexes[0], task_value, &mut context.space) }
    Ok(())
}

fn park(_: ValuesView, _: &[ValueIndex], _: &mut Eval, _: &Asset) -> Result<(), ExecuteError> {
    std::thread::park();
    Ok(())
}

struct String {
    buf: SpaceAddr,
    len: usize,
    // TODO add cap
}

impl Value {
    pub fn alloc_string(literal: &str, space: &mut Space) -> Result<Self, OutOfSpace> {
        let len = literal.len();
        // allocate buffer first, otherwise if buffer fails to allocate while String
        // itself succeeds, marking String would encounter invalid SpaceAddr
        let buf = space.alloc(Layout::from_size_align(len, 1).unwrap())?;
        unsafe { space.get_mut(buf, len) }.copy_from_slice(literal.as_bytes());
        let addr = space.typed_alloc::<String>()?;
        unsafe { space.typed_write(addr, String { buf, len }) }
        Ok(Self::new(TypeId::STRING, addr))
    }

    fn get_str<'a>(&self, space: &'a Space) -> Result<&'a str, TypeError> {
        self.ensure_type(TypeId::STRING)?;
        let string = unsafe { space.typed_get::<String>(self.addr()) };
        Ok(unsafe { str::from_utf8_unchecked(space.get(string.buf, string.len)) })
    }
}

unsafe fn trace(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    asset: &Asset,
) -> Result<(), ExecuteError> {
    let value = unsafe { values.load(indexes[0], &context.space) };

    fn format_value(
        value: Value,
        top: bool,
        space: &Space,
        registry: &TypeRegistry,
        asset: &Asset,
    ) -> std::string::String {
        if let Ok(message) = value.get_str(space) {
            message.into()
        } else if let Ok(int32) = value.load_int32() {
            int32.to_string()
        } else if let Some(RecordLayout(attrs)) = registry.get_record_layout(value.type_id()) {
            let attrs = if top {
                attrs
                    .iter()
                    .enumerate()
                    .map(|(i, &attr)| {
                        let attr_value = unsafe { ValuesView(value.addr()).load(i, space) };
                        format!(
                            "{} = {}",
                            asset.get_string(attr),
                            format_value(attr_value, false, space, registry, asset)
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            } else {
                "...".into()
            };
            format!("record({:?})[{attrs}]", value.type_id())
        } else {
            "<unknown>".into() // TODO
        }
    }

    tracing::info!(
        "{}",
        format_value(value, true, &context.space, &context.registry, asset)
    );
    Ok(())
}

unsafe fn type_object(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let id = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
    unsafe {
        values.store(
            indexes[0],
            Value::new_type_id(TypeId(id as _)),
            &mut context.space,
        )
    }
    Ok(())
}

thread_local!(static START: std::time::Instant = std::time::Instant::now());
unsafe fn time_since_start(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let duration = START.with(|start| start.elapsed());
    assert!(
        duration.as_secs() < i32::MAX as _,
        "program has been running for too long"
    );
    unsafe {
        values.store(
            indexes[0],
            Value::new_int32(duration.as_secs() as _),
            &mut context.space,
        );
        values.store(
            indexes[1],
            Value::new_int32(duration.subsec_nanos() as _),
            &mut context.space,
        )
    }
    Ok(())
}

#[derive(Debug, Clone, Copy)]
struct List {
    buf: SpaceAddr,
    // these are kept natively (instead of as record attributes) for garbage
    // collection purposes. `len` is for recursing into referenced values, `cap` is
    // reset to `len` after the collection, which only allocate and copy up to `len`
    len: usize,
    cap: usize,
}

impl Value {
    fn alloc_list(cap: usize, space: &mut Space) -> Result<Self, OutOfSpace> {
        let buf = space.alloc(Layout::array::<Value>(cap).unwrap())?;
        let addr = space.typed_alloc::<List>()?;
        unsafe { space.typed_write(addr, List { buf, len: 0, cap }) }
        Ok(Self::new(TypeId::LIST, addr))
    }

    // it is also ok to have a load_list
    fn get_list<'a>(&self, space: &'a Space) -> Result<&'a List, TypeError> {
        self.ensure_type(TypeId::LIST)?;
        Ok(unsafe { space.typed_get::<List>(self.addr()) })
    }

    fn get_list_mut<'a>(&self, space: &'a mut Space) -> Result<&'a mut List, TypeError> {
        self.ensure_type(TypeId::LIST)?;
        Ok(unsafe { space.typed_get_mut::<List>(self.addr()) })
    }
}

unsafe fn list_new(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let cap = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
    if cap < 0 {
        todo!()
    }
    let list_value = Value::alloc_list(cap as _, &mut context.space)?;
    unsafe { values.store(indexes[0], list_value, &mut context.space) }
    Ok(())
}

// list_push is implemented from lang side, with the following intrinsics
unsafe fn list_len(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
    unsafe {
        values.store(
            indexes[0],
            Value::new_int32(list.len as _),
            &mut context.space,
        )
    }
    Ok(())
}

unsafe fn list_set_len(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let len = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
    let list =
        unsafe { values.load(indexes[0], &context.space) }.get_list_mut(&mut context.space)?;
    assert!(len >= 0);
    assert!(len as usize <= list.cap);
    list.len = len as _;
    Ok(())
}

unsafe fn list_cap(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
    unsafe {
        values.store(
            indexes[0],
            Value::new_int32(list.cap as _),
            &mut context.space,
        )
    }
    Ok(())
}

// while this can be achieved by per-value load + store, expect a performance
// gap to necessitate
unsafe fn list_copy(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let src = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
    let dst = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
    let len = src.len;
    assert!(dst.cap >= len);
    let src_buf = src.buf;
    let dst_buf = dst.buf;
    assert_ne!(src_buf, dst_buf);
    unsafe {
        copy_nonoverlapping(
            context.space.typed_get::<Value>(src_buf),
            context.space.typed_get_mut(dst_buf),
            len,
        )
    }
    Ok(())
}

unsafe fn list_load(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(indexes[1], &context.space) }.get_list(&context.space)?;
    let pos = unsafe { values.load(indexes[2], &context.space) }.load_int32()?;
    assert!(pos >= 0);
    assert!((pos as usize) < list.len); // avoid exposing garbage value to lang
    let value = unsafe { ValuesView(list.buf).load(pos as _, &context.space) };
    unsafe { values.store(indexes[0], value, &mut context.space) }
    Ok(())
}

unsafe fn list_store(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
    let pos = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
    assert!(pos >= 0);
    assert!((pos as usize) < list.cap); // as long as not overflowing it is fine
    let value = unsafe { values.load(indexes[2], &context.space) };
    unsafe { ValuesView(list.buf).store(pos as _, value, &mut context.space) };
    Ok(())
}

// similar to list_copy but for a performant list_insert and list_remove
unsafe fn list_copy_within(
    values: ValuesView,
    indexes: &[ValueIndex],
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(indexes[0], &context.space) }.get_list(&context.space)?;
    let src = unsafe { values.load(indexes[1], &context.space) }.load_int32()?;
    let len = unsafe { values.load(indexes[2], &context.space) }.load_int32()?;
    let dst = unsafe { values.load(indexes[3], &context.space) }.load_int32()?;
    assert!(src >= 0);
    assert!(dst >= 0);
    assert!(len > 0);
    let src = src as usize;
    let dst = dst as usize;
    let len = len as usize;
    assert!(src + len <= list.len);
    assert!(dst + len <= list.len);
    unsafe {
        copy(
            context
                .space
                .typed_get::<Value>(list.buf.add(src * size_of::<Value>())),
            context
                .space
                .typed_get_mut(list.buf.add(dst * size_of::<Value>())),
            len,
        );
    }
    Ok(())
}
