use std::{
    alloc::{Layout, alloc},
    ops::{Index, IndexMut},
    ptr::slice_from_raw_parts_mut,
};

pub const MIN_CAP: usize = 4;

pub struct MyVec<T> {
    slice: *mut [T],
    size: usize,
}

impl<T> MyVec<T> {
    pub fn new() -> Self {
        Self::with_capacity(MIN_CAP)
    }

    pub fn with_capacity(cap: usize) -> Self {
        assert!(cap != 0);
        assert!(size_of::<T>() != 0);

        let ptr = unsafe { alloc(Layout::array::<T>(cap).unwrap()) } as *mut T;

        Self {
            slice: slice_from_raw_parts_mut(ptr, cap),
            size: 0,
        }
    }

    fn extend(&mut self) {
        let new_cap = self.slice.len() * 2;

        let new_ptr = unsafe { alloc(Layout::array::<T>(new_cap).unwrap()) } as *mut T;

        unsafe {
            std::ptr::copy_nonoverlapping(self.slice as *mut T, new_ptr, self.slice.len());
        };

        let new_slice = slice_from_raw_parts_mut(new_ptr, new_cap);

        unsafe {
            std::alloc::dealloc(
                self.slice as *mut u8,
                Layout::array::<T>(self.slice.len()).unwrap(),
            );
        }

        self.slice = new_slice;
    }

    pub fn push(&mut self, v: T) {
        if self.size == self.slice.len() {
            self.extend();
        }

        unsafe {
            (*self.slice)[self.size] = v;
        }

        self.size += 1;
    }
}

impl<T> Index<usize> for MyVec<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.size);

        unsafe { &(*self.slice)[index] }
    }
}

impl<T> IndexMut<usize> for MyVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.size);

        unsafe { &mut (*self.slice)[index] }
    }
}
