use std::{
    alloc::Layout,
    ptr::{copy, copy_nonoverlapping},
    slice,
};

use thiserror::Error;

use crate::{
    asset::Asset,
    code::instr::Intrinsic,
    eval::{Eval, ExecuteError, IntrinsicValues, Task, TypeError, Value},
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
    values: IntrinsicValues<'_>,
    context: &mut Eval,
    asset: &Asset,
) -> Result<(), ExecuteError> {
    let closure = unsafe { values.load(1).load_closure() }?;
    let task_value = Value::alloc_task(&mut context.space)?;
    unsafe { task_value.addr().cast::<Task>().as_mut() }.push_frame(
        closure,
        &[],
        &mut context.space,
        asset,
    )?;
    values.store(0, task_value);
    Ok(())
}

fn park(_: IntrinsicValues<'_>, _: &mut Eval, _: &Asset) -> Result<(), ExecuteError> {
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
        unsafe { copy_nonoverlapping(literal.as_ptr(), buf.as_ptr(), len) }
        let addr = space.typed_alloc::<String>()?;
        unsafe { addr.cast::<String>().write(String { buf, len }) }
        Ok(Self::new(TypeId::STRING, addr))
    }

    fn get_str<'a>(self) -> Result<&'a str, TypeError> {
        self.ensure_type(TypeId::STRING)?;
        let string = unsafe { self.addr().cast::<String>().as_ref() };
        Ok(unsafe {
            str::from_utf8_unchecked(slice::from_raw_parts(string.buf.as_ptr(), string.len))
        })
    }
}

unsafe fn trace(
    values: IntrinsicValues<'_>,
    context: &mut Eval,
    asset: &Asset,
) -> Result<(), ExecuteError> {
    fn format_value(
        value: Value,
        top: bool,
        registry: &TypeRegistry,
        asset: &Asset,
    ) -> std::string::String {
        if let Ok(message) = value.get_str() {
            message.into()
        } else if let Ok(int32) = value.load_int32() {
            int32.to_string()
        } else if let Some(RecordLayout(attrs)) = registry.get_record_layout(value.type_id()) {
            let attr_values = value.addr().cast::<Value>();
            let attrs = if !top {
                "...".into()
            } else {
                attrs
                    .iter()
                    .enumerate()
                    .map(|(i, &attr)| {
                        let attr_value = unsafe { attr_values.add(i).read() };
                        let attr_fmt = format_value(attr_value, false, registry, asset);
                        format!("{} = {attr_fmt}", asset.get_string(attr))
                    })
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            format!("record({:?})[{attrs}]", value.type_id())
        } else {
            "<unknown>".into() // TODO
        }
    }

    tracing::info!(
        "{}",
        format_value(values.load(0), true, &context.registry, asset)
    );
    Ok(())
}

unsafe fn type_object(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let id = values.load(1).load_int32()?;
    values.store(0, Value::new_type_id(TypeId(id as _)));
    Ok(())
}

thread_local!(static START: std::time::Instant = std::time::Instant::now());
unsafe fn time_since_start(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let duration = START.with(|start| start.elapsed());
    assert!(
        duration.as_secs() < i32::MAX as _,
        "program has been running for too long"
    );
    values.store(0, Value::new_int32(duration.as_secs() as _));
    values.store(1, Value::new_int32(duration.subsec_nanos() as _));
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
        unsafe { addr.cast::<List>().write(List { buf, len: 0, cap }) }
        Ok(Self::new(TypeId::LIST, addr))
    }

    // it is also ok to have a load_list
    unsafe fn get_list<'a>(self) -> Result<&'a List, TypeError> {
        self.ensure_type(TypeId::LIST)?;
        Ok(unsafe { self.addr().cast::<List>().as_ref() })
    }

    unsafe fn get_list_mut<'a>(self) -> Result<&'a mut List, TypeError> {
        self.ensure_type(TypeId::LIST)?;
        Ok(unsafe { self.addr().cast::<List>().as_mut() })
    }
}

unsafe fn list_new(
    values: IntrinsicValues<'_>,
    context: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let cap = values.load(1).load_int32()?;
    assert!(cap >= 0);
    let list_value = Value::alloc_list(cap as _, &mut context.space)?;
    values.store(0, list_value);
    Ok(())
}

// list_push is implemented from lang side, with the following intrinsics
unsafe fn list_len(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(1).get_list() }?;
    values.store(0, Value::new_int32(list.len as _));
    Ok(())
}

unsafe fn list_cap(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(1).get_list() }?;
    values.store(0, Value::new_int32(list.cap as _));
    Ok(())
}

unsafe fn list_set_len(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(0).get_list_mut()? };
    let len = values.load(1).load_int32()?;
    assert!(len >= 0);
    assert!(len as usize <= list.cap);
    list.len = len as _;
    Ok(())
}

// while this can be achieved by per-value load + store, expect a performance
// gap to necessitate
unsafe fn list_copy(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let src = unsafe { values.load(0).get_list() }?;
    let dst = unsafe { values.load(1).get_list() }?;
    assert_ne!(src.buf, dst.buf);
    let len = src.len;
    assert!(len > 0);
    assert!(dst.cap >= len);
    unsafe {
        copy_nonoverlapping(
            src.buf.cast::<Value>().as_ptr(),
            dst.buf.cast().as_ptr(),
            len,
        )
    }
    Ok(())
}

unsafe fn list_load(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(1).get_list() }?;
    let pos = values.load(2).load_int32()?;
    assert!(pos >= 0);
    assert!((pos as usize) < list.len); // avoid exposing garbage value to lang
    values.store(0, unsafe { list.buf.cast::<Value>().add(pos as _).read() });
    Ok(())
}

unsafe fn list_store(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(0).get_list() }?;
    let pos = values.load(1).load_int32()?;
    assert!(pos >= 0);
    assert!((pos as usize) < list.cap); // as long as not overflowing this is fine
    let value = values.load(2);
    unsafe { list.buf.cast::<Value>().add(pos as _).write(value) }
    Ok(())
}

// similar to list_copy but for a performant list_insert and list_remove
unsafe fn list_copy_within(
    values: IntrinsicValues<'_>,
    _: &mut Eval,
    _: &Asset,
) -> Result<(), ExecuteError> {
    let list = unsafe { values.load(0).get_list() }?;
    let src = values.load(1).load_int32()?;
    let len = values.load(2).load_int32()?;
    let dst = values.load(3).load_int32()?;

    assert!(src >= 0);
    assert!(dst >= 0);
    assert!(len > 0);
    let src = src as usize;
    let dst = dst as usize;
    let len = len as usize;
    assert!(src + len <= list.len);
    assert!(dst + len <= list.len);

    let buf = list.buf.cast::<Value>();
    unsafe { copy(buf.add(src).as_ptr(), buf.add(dst).as_ptr(), len) }
    Ok(())
}
