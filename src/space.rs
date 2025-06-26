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

    pub fn alloc<T: Sized>(&mut self) -> SpaceAddr {
        let addr = self.alloc_addr
            + unsafe { self.buf.as_ptr().add(self.alloc_addr) }.align_offset(align_of::<T>());
        if addr + size_of::<T>() > self.buf.len() {
            // TODO
        }
        self.alloc_addr = addr + size_of::<T>();
        addr
    }

    pub unsafe fn get<T>(&self, addr: SpaceAddr) -> &T {
        assert!(addr < self.alloc_addr);
        let addr = unsafe { self.buf.as_ptr().add(addr).cast::<T>() };
        assert!(addr.is_aligned());
        unsafe { &*addr }
    }

    pub unsafe fn write<T>(&mut self, addr: SpaceAddr, value: T) {
        assert!(addr < self.alloc_addr);
        let addr = unsafe { self.buf.as_mut_ptr().add(addr).cast::<T>() };
        assert!(addr.is_aligned());
        unsafe { addr.write(value) }
    }
}
