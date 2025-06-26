use std::alloc::Layout;

#[derive(Default)]
pub struct Space {
    buf: Vec<u8>,
    alloc_addr: SpaceAddr,
}

pub type SpaceAddr = usize;

impl Space {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc<T: Sized>(&mut self) -> SpaceAddr {
        let layout = Layout::new::<T>();
        let offset = self.alloc_addr.next_multiple_of(layout.align());
        if offset + layout.size() > self.buf.len() {
            // TODO
        }
        self.alloc_addr = offset + layout.size();
        offset
    }

    pub unsafe fn get<T>(&self, addr: SpaceAddr) -> &T {
        let layout = Layout::new::<T>();
        assert!(addr < self.alloc_addr);
        assert!(addr.is_multiple_of(layout.align()));
        unsafe { &*(self.buf.as_ptr().add(addr) as *const T) }
    }

    pub unsafe fn write<T>(&mut self, addr: SpaceAddr, value: T) {
        let layout = Layout::new::<T>();
        assert!(addr < self.alloc_addr);
        assert!(addr.is_multiple_of(layout.align()));
        unsafe { (self.buf.as_mut_ptr().add(addr) as *mut T).write(value) }
    }
}
