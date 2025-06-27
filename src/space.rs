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

    pub fn alloc(&mut self, size: usize, align: usize) -> SpaceAddr {
        let addr = self.alloc_addr + (&self.buf[self.alloc_addr] as *const u8).align_offset(align);
        if addr + size > self.buf.len() {
            // TODO
        }
        self.alloc_addr = addr + size;
        addr
    }

    pub fn get(&self, addr: SpaceAddr, size: usize) -> &[u8] {
        assert!(addr + size <= self.alloc_addr);
        &self.buf[addr..addr + size]
    }

    pub fn get_mut(&mut self, addr: SpaceAddr, size: usize) -> &mut [u8] {
        assert!(addr + size <= self.alloc_addr);
        &mut self.buf[addr..addr + size]
    }

    pub fn typed_alloc<T>(&mut self) -> SpaceAddr {
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
    pub unsafe fn typed_write<T>(&mut self, addr: SpaceAddr, value: T) {
        let addr = self.get_mut(addr, size_of::<T>()).as_mut_ptr().cast::<T>();
        assert!(addr.is_aligned());
        unsafe { addr.write(value) }
    }
}
