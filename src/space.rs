use std::{alloc::Layout, ptr::NonNull, slice};

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

impl SpaceAddr {
    pub unsafe fn add(self, count: usize) -> Self {
        Self(unsafe { self.0.add(count) })
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

    pub unsafe fn alloc_copy<T: Default + Copy>(
        &mut self,
        new_cap: usize,
        SpaceAddr(src_values): SpaceAddr,
        len: usize,
    ) -> Result<SpaceAddr, OutOfSpace> {
        let new_values = self.bump.alloc_slice_fill_default::<T>(new_cap);
        if len > 0 {
            let src_values = unsafe { slice::from_raw_parts(src_values.cast().as_ptr(), len) };
            new_values[..len].copy_from_slice(src_values)
        }
        Ok(SpaceAddr(NonNull::from(new_values).cast()))
    }

    #[allow(unused)]
    pub fn copy_collect(&mut self, OutOfSpace(requested_size): OutOfSpace, addr: SpaceAddr) {
        todo!()
    }

    /// # Safety
    /// * `addr` must be obtained by calling `alloc` with a layout whose size is
    /// `size`.
    /// * `addr` must be reachable from every previous root `addr` of `copy_collect`
    /// calls
    pub unsafe fn get(&self, SpaceAddr(addr): SpaceAddr, size: usize) -> &[u8] {
        unsafe { slice::from_raw_parts(addr.as_ptr(), size) }
    }

    /// # Safety
    /// Same as `get`.
    pub unsafe fn get_mut(&mut self, SpaceAddr(addr): SpaceAddr, size: usize) -> &mut [u8] {
        unsafe { slice::from_raw_parts_mut(addr.as_ptr(), size) }
    }

    pub fn typed_alloc<T>(&mut self) -> Result<SpaceAddr, OutOfSpace> {
        self.alloc(Layout::new::<T>())
    }

    /// # Safety
    /// Same as `get`, plus the caller must ensure `addr` contains a `T` instance,
    /// e.g., was `typed_alloc::<T>`ed
    pub unsafe fn typed_get<T>(&self, addr: SpaceAddr) -> &T {
        let addr = unsafe { self.get(addr, size_of::<T>()) }
            .as_ptr()
            .cast::<T>();
        assert!(addr.is_aligned());
        unsafe { &*addr }
    }

    /// # Safety
    /// Same as `typed_get`.
    pub unsafe fn typed_get_mut<T>(&mut self, addr: SpaceAddr) -> &mut T {
        let addr = unsafe { self.get_mut(addr, size_of::<T>()) }
            .as_mut_ptr()
            .cast::<T>();
        assert!(addr.is_aligned());
        unsafe { &mut *addr }
    }

    /// # Safety
    /// Same as `typed_get`.
    pub unsafe fn typed_write<T>(&mut self, addr: SpaceAddr, value: T) {
        let addr = unsafe { self.get_mut(addr, size_of::<T>()) }
            .as_mut_ptr()
            .cast::<T>();
        assert!(addr.is_aligned());
        unsafe { addr.write(value) }
    }
}

impl Drop for Space {
    fn drop(&mut self) {
        tracing::debug!("on exit: {} bytes allocated", self.bump.allocated_bytes())
    }
}
