use thiserror::Error;

#[derive(Default)]
pub struct Space {
    buf: Vec<u8>,
    alloc_addr: SpaceAddr,
}

pub type SpaceAddr = usize;

impl Space {
    pub fn new(buf_size: usize) -> Self {
        Self {
            buf: vec![0; buf_size],
            alloc_addr: 0,
        }
    }
}

#[derive(Debug, Error)]
#[error("running out of space while requesting {0} bytes")]
pub struct OutOfSpace(pub usize);

impl Space {
    pub fn alloc(&mut self, size: usize, align: usize) -> Result<SpaceAddr, OutOfSpace> {
        let addr = self.alloc_addr + (&self.buf[self.alloc_addr] as *const u8).align_offset(align);
        if addr + size > self.buf.len() {
            return Err(OutOfSpace(size));
        }
        self.alloc_addr = addr + size;
        Ok(addr)
    }

    #[allow(unused)]
    pub fn copy_collect(&mut self, OutOfSpace(requested_size): OutOfSpace, addr: SpaceAddr) {
        todo!()
    }

    pub fn get(&self, addr: SpaceAddr, size: usize) -> &[u8] {
        assert!(addr + size <= self.alloc_addr);
        &self.buf[addr..addr + size]
    }

    pub fn get_mut(&mut self, addr: SpaceAddr, size: usize) -> &mut [u8] {
        assert!(addr + size <= self.alloc_addr);
        &mut self.buf[addr..addr + size]
    }

    pub fn typed_alloc<T>(&mut self) -> Result<SpaceAddr, OutOfSpace> {
        self.alloc(size_of::<T>(), align_of::<T>())
    }

    /// # Safety
    /// The caller must ensure `addr` contains a `T` instance, e.g., was
    /// `typed_alloc::<T>`ed
    pub unsafe fn typed_get<T>(&self, addr: SpaceAddr) -> &T {
        let addr = self.get(addr, size_of::<T>()).as_ptr().cast::<T>();
        assert!(addr.is_aligned());
        unsafe { &*addr }
    }

    /// # Safety
    /// Same as `typed_get`.
    pub unsafe fn typed_get_mut<T>(&mut self, addr: SpaceAddr) -> &mut T {
        let addr = self.get_mut(addr, size_of::<T>()).as_mut_ptr().cast::<T>();
        assert!(addr.is_aligned());
        unsafe { &mut *addr }
    }

    /// # Safety
    /// Same as `typed_get`.
    pub unsafe fn typed_write<T>(&mut self, addr: SpaceAddr, value: T) {
        let addr = self.get_mut(addr, size_of::<T>()).as_mut_ptr().cast::<T>();
        assert!(addr.is_aligned());
        unsafe { addr.write(value) }
    }
}

impl Drop for Space {
    fn drop(&mut self) {
        tracing::debug!("on exit: {} bytes allocated", self.alloc_addr)
    }
}
