use std::{alloc::Layout, ops::Deref, ptr::NonNull};

use bumpalo::{AllocErr, Bump};
use thiserror::Error;

#[derive(Default)]
pub struct Space {
    bump: Bump<8>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SpaceAddr(pub NonNull<u8>);

impl Default for SpaceAddr {
    fn default() -> Self {
        Self(NonNull::dangling())
    }
}

impl Deref for SpaceAddr {
    type Target = NonNull<u8>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Space {
    pub fn new(limit: impl Into<Option<usize>>) -> Self {
        let bump = Bump::<8>::with_min_align();
        bump.set_allocation_limit(limit.into());
        Self { bump }
    }
}

#[derive(Debug, Error)]
#[error("running out of space while requesting {0} bytes")]
pub struct OutOfSpace(pub usize);

impl Space {
    pub fn alloc(&mut self, layout: Layout) -> Result<SpaceAddr, OutOfSpace> {
        match self.bump.try_alloc_layout(layout) {
            Ok(addr) => Ok(SpaceAddr(addr)),
            Err(AllocErr) => Err(OutOfSpace(layout.size())),
        }
    }

    pub fn typed_alloc<T>(&mut self) -> Result<SpaceAddr, OutOfSpace> {
        self.alloc(Layout::new::<T>())
    }

    #[allow(unused)]
    pub fn copy_collect(&mut self, OutOfSpace(requested_size): OutOfSpace, addr: SpaceAddr) {
        todo!()
    }
}

impl Drop for Space {
    fn drop(&mut self) {
        tracing::debug!("on exit: {} bytes allocated", self.bump.allocated_bytes())
    }
}
