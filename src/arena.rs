//! Based on `typed_arena` crate

#![allow(clippy::mut_from_ref)]
use core::cmp;
use core::iter;
use core::mem;
use core::ptr;
use core::slice;
use core::str;
use std::sync::Mutex;

use mem::MaybeUninit;

// Initial size in bytes.
const INITIAL_SIZE: usize = 1024;
// Minimum capacity. Must be larger than 0.
const MIN_CAPACITY: usize = 1;

/// An arena of objects of type `T`.
pub struct Arena<T> {
    chunks: Mutex<ChunkList<T>>,
}

struct ChunkList<T> {
    current: Vec<T>,
    rest: Vec<Vec<T>>,
}

impl<T> Arena<T> {
    /// Construct a new arena.
    pub fn new() -> Arena<T> {
        let size = cmp::max(1, mem::size_of::<T>());
        Arena::with_capacity(INITIAL_SIZE / size)
    }

    /// Construct a new arena with capacity for `n` values pre-allocated.
    pub fn with_capacity(n: usize) -> Arena<T> {
        let n = cmp::max(MIN_CAPACITY, n);
        Arena {
            chunks: Mutex::new(ChunkList {
                current: Vec::with_capacity(n),
                rest: Vec::new(),
            }),
        }
    }

    /// Return the size of the arena
    ///
    /// This is useful for using the size of previous typed arenas to build new typed arenas with large enough spaces.
    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        let chunks = self.chunks.lock().unwrap();

        let mut res = 0;
        for vec in chunks.rest.iter() {
            res += vec.len()
        }

        res + chunks.current.len()
    }

    /// Allocates a value in the arena, and returns a mutable reference
    /// to that value.
    #[inline]
    pub fn alloc(&self, value: T) -> &mut T {
        self.alloc_fast_path(value)
            .unwrap_or_else(|value| self.alloc_slow_path(value))
    }

    #[inline]
    fn alloc_fast_path(&self, value: T) -> Result<&mut T, T> {
        let mut chunks = self.chunks.lock().unwrap();
        let len = chunks.current.len();
        if len < chunks.current.capacity() {
            chunks.current.push(value);
            // Avoid going through `Vec::deref_mut`, which overlaps
            // other references we have already handed out!
            debug_assert!(len < chunks.current.len()); // bounds check
            Ok(unsafe { &mut *chunks.current.as_mut_ptr().add(len) })
        } else {
            Err(value)
        }
    }

    fn alloc_slow_path(&self, value: T) -> &mut T {
        &mut self.alloc_extend(iter::once(value))[0]
    }

    /// Uses the contents of an iterator to allocate values in the arena.
    /// Returns a mutable slice that contains these values.
    pub fn alloc_extend<I>(&self, iterable: I) -> &mut [T]
    where
        I: IntoIterator<Item = T>,
    {
        let mut iter = iterable.into_iter();

        let mut chunks = self.chunks.lock().unwrap();

        let iter_min_len = iter.size_hint().0;
        let mut next_item_index;
        debug_assert!(
            chunks.current.capacity() >= chunks.current.len(),
            "capacity is always greater than or equal to len, so we don't need to worry about underflow"
        );
        if iter_min_len > chunks.current.capacity() - chunks.current.len() {
            chunks.reserve(iter_min_len);
            chunks.current.extend(iter);
            next_item_index = 0;
        } else {
            next_item_index = chunks.current.len();
            let mut i = 0;
            while let Some(elem) = iter.next() {
                if chunks.current.len() == chunks.current.capacity() {
                    // The iterator was larger than we could fit into the current chunk.
                    let chunks = &mut *chunks;
                    // Create a new chunk into which we can freely push the entire iterator into
                    chunks.reserve(i + 1);
                    let previous_chunk = chunks.rest.last_mut().unwrap();
                    let previous_chunk_len = previous_chunk.len();
                    // Move any elements we put into the previous chunk into this new chunk
                    chunks
                        .current
                        .extend(previous_chunk.drain(previous_chunk_len - i..));
                    chunks.current.push(elem);
                    // And the remaining elements in the iterator
                    chunks.current.extend(iter);
                    next_item_index = 0;
                    break;
                } else {
                    chunks.current.push(elem);
                }
                i += 1;
            }
        }

        // Extend the lifetime from that of `chunks_borrow` to that of `self`.
        // This is OK because we’re careful to never move items
        // by never pushing to inner `Vec`s beyond their initial capacity.
        // The returned reference is unique (`&mut`):
        // the `Arena` never gives away references to existing items.
        unsafe {
            let new_len = chunks.current.len() - next_item_index;
            slice::from_raw_parts_mut(chunks.current.as_mut_ptr().add(next_item_index), new_len)
        }
    }

    /// Makes sure there's enough continuous space for at least `num` elements.
    ///
    /// This may save some work if called before [`alloc_extend`][Arena::alloc_extend]. It also
    /// allows somewhat safer use pattern of [`alloc_uninitialized`][Arena::alloc_uninitialized].
    /// On the other hand this might waste up to `n - 1` elements of space. In case new allocation
    /// is needed, the unused ones in current chunk are never used.
    pub fn reserve_extend(&self, num: usize) {
        let mut chunks = self.chunks.lock().unwrap();

        debug_assert!(
            chunks.current.capacity() >= chunks.current.len(),
            "capacity is always greater than or equal to len, so we don't need to worry about underflow"
        );
        if num > chunks.current.capacity() - chunks.current.len() {
            chunks.reserve(num);
        }
    }

    /// Returns unused space.
    ///
    /// *This unused space is still not considered "allocated".* Therefore, it
    /// won't be dropped unless there are further calls to `alloc`,
    /// [`alloc_uninitialized`][Arena::alloc_uninitialized], or
    /// [`alloc_extend`][Arena::alloc_extend] which is why the method is safe.
    ///
    /// It returns a raw pointer to avoid creating multiple mutable references to the same place.
    /// It is up to the caller not to dereference it after any of the `alloc_` methods are called.
    pub fn uninitialized_array(&self) -> *mut [MaybeUninit<T>] {
        let mut chunks = self.chunks.lock().unwrap();
        let len = chunks.current.capacity() - chunks.current.len();
        let next_item_index = chunks.current.len();

        unsafe {
            // Go through pointers, to make sure we never create a reference to uninitialized T.
            let start = chunks.current.as_mut_ptr().add(next_item_index);
            let start_uninit = start as *mut MaybeUninit<T>;
            ptr::slice_from_raw_parts_mut(start_uninit, len)
        }
    }

    /// Convert this `Arena` into a `Vec<T>`.
    ///
    /// Items in the resulting `Vec<T>` appear in the order that they were
    /// allocated in.
    pub fn into_vec(self) -> Vec<T> {
        let mut chunks = self.chunks.into_inner().unwrap();
        // keep order of allocation in the resulting Vec
        let n = chunks
            .rest
            .iter()
            .fold(chunks.current.len(), |a, v| a + v.len());
        let mut result = Vec::with_capacity(n);
        for mut vec in chunks.rest {
            result.append(&mut vec);
        }
        result.append(&mut chunks.current);
        result
    }

    /// Returns an iterator that allows modifying each value.
    ///
    /// Items are yielded in the order that they were allocated.
    ///
    /// ## Immutable Iteration
    ///
    /// Note that there is no corresponding `iter` method. Access to the arena's contents
    /// requries mutable access to the arena itself.
    ///
    /// ```compile_fail
    /// use crate::arena::Arena;
    ///
    /// let mut arena = Arena::new();
    /// let x = arena.alloc(1);
    ///
    /// // borrow error!
    /// for i in arena.iter_mut() {
    ///     println!("i: {}", i);
    /// }
    ///
    /// // borrow error!
    /// *x = 2;
    /// ```
    #[allow(clippy::missing_transmute_annotations)]
    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<T> {
        let chunks = self.chunks.get_mut().unwrap();
        let position = if !chunks.rest.is_empty() {
            let index = 0;
            let inner_iter = chunks.rest[index].iter_mut();
            // Extend the lifetime of the individual elements to that of the arena.
            // This is OK because we borrow the arena mutably to prevent new allocations
            // and we take care here to never move items inside the arena while the
            // iterator is alive.
            let inner_iter = unsafe { mem::transmute(inner_iter) };
            IterMutState::ChunkListRest { index, inner_iter }
        } else {
            // Extend the lifetime of the individual elements to that of the arena.
            let iter = unsafe { mem::transmute(chunks.current.iter_mut()) };
            IterMutState::ChunkListCurrent { iter }
        };
        IterMut {
            chunks,
            state: position,
        }
    }
}

impl Arena<u8> {
    /// Allocates a string slice and returns a mutable reference to it.
    ///
    /// This is on `Arena<u8>`, because string slices use byte slices (`[u8]`) as their backing
    /// storage.
    #[inline]
    pub fn alloc_str(&self, s: &str) -> &mut str {
        let buffer = self.alloc_extend(s.bytes());
        // Can't fail the utf8 validation, it already came in as utf8
        unsafe { str::from_utf8_unchecked_mut(buffer) }
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> ChunkList<T> {
    #[inline(never)]
    #[cold]
    fn reserve(&mut self, additional: usize) {
        let double_cap = self
            .current
            .capacity()
            .checked_mul(2)
            .expect("capacity overflow");
        let required_cap = additional
            .checked_next_power_of_two()
            .expect("capacity overflow");
        let new_capacity = cmp::max(double_cap, required_cap);
        let chunk = mem::replace(&mut self.current, Vec::with_capacity(new_capacity));
        self.rest.push(chunk);
    }
}

enum IterMutState<'a, T> {
    ChunkListRest {
        index: usize,
        inner_iter: slice::IterMut<'a, T>,
    },
    ChunkListCurrent {
        iter: slice::IterMut<'a, T>,
    },
}

/// Mutable arena iterator.
///
/// This struct is created by the [`iter_mut`](struct.Arena.html#method.iter_mut) method on [Arenas](struct.Arena.html).
pub struct IterMut<'a, T: 'a> {
    chunks: &'a mut ChunkList<T>,
    state: IterMutState<'a, T>,
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;

    #[allow(clippy::missing_transmute_annotations)]
    fn next(&mut self) -> Option<&'a mut T> {
        loop {
            self.state = match self.state {
                IterMutState::ChunkListRest {
                    mut index,
                    ref mut inner_iter,
                } => {
                    match inner_iter.next() {
                        Some(item) => return Some(item),
                        None => {
                            index += 1;
                            if index < self.chunks.rest.len() {
                                let inner_iter = self.chunks.rest[index].iter_mut();
                                // Extend the lifetime of the individual elements to that of the arena.
                                let inner_iter = unsafe { mem::transmute(inner_iter) };
                                IterMutState::ChunkListRest { index, inner_iter }
                            } else {
                                let iter = self.chunks.current.iter_mut();
                                // Extend the lifetime of the individual elements to that of the arena.
                                let iter = unsafe { mem::transmute(iter) };
                                IterMutState::ChunkListCurrent { iter }
                            }
                        }
                    }
                }
                IterMutState::ChunkListCurrent { ref mut iter } => return iter.next(),
            };
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let current_len = self.chunks.current.len();
        let current_cap = self.chunks.current.capacity();
        if self.chunks.rest.is_empty() {
            (current_len, Some(current_len))
        } else {
            let rest_len = self.chunks.rest.len();
            let last_chunk_len = self
                .chunks
                .rest
                .last()
                .map(|chunk| chunk.len())
                .unwrap_or(0);

            let min = current_len + last_chunk_len;
            let max = min + (rest_len * current_cap / rest_len);

            (min, Some(max))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::cell::Cell;
    use std::panic::{self, AssertUnwindSafe};
    use std::ptr;

    struct DropTracker<'a>(&'a Cell<u32>);
    impl Drop for DropTracker<'_> {
        fn drop(&mut self) {
            self.0.set(self.0.get() + 1);
        }
    }

    #[allow(unused)]
    struct Node<'a, 'b: 'a>(Option<&'a Node<'a, 'b>>, u32, DropTracker<'b>);

    #[test]
    fn arena_as_intended() {
        let drop_counter = Cell::new(0);
        {
            let arena = Arena::with_capacity(2);
            {
                let mut node: &Node = arena.alloc(Node(None, 1, DropTracker(&drop_counter)));
                assert_eq!(arena.chunks.lock().unwrap().rest.len(), 0);

                node = arena.alloc(Node(Some(node), 2, DropTracker(&drop_counter)));
                assert_eq!(arena.chunks.lock().unwrap().rest.len(), 0);

                node = arena.alloc(Node(Some(node), 3, DropTracker(&drop_counter)));
                assert_eq!(arena.chunks.lock().unwrap().rest.len(), 1);

                node = arena.alloc(Node(Some(node), 4, DropTracker(&drop_counter)));
                assert_eq!(arena.chunks.lock().unwrap().rest.len(), 1);

                assert_eq!(node.1, 4);
                assert_eq!(node.0.unwrap().1, 3);
                assert_eq!(node.0.unwrap().0.unwrap().1, 2);
                assert_eq!(node.0.unwrap().0.unwrap().0.unwrap().1, 1);
                assert!(node.0.unwrap().0.unwrap().0.unwrap().0.is_none());

                assert_eq!(arena.len(), 4);
            }

            assert_eq!(drop_counter.get(), 0);

            let mut node: &Node = arena.alloc(Node(None, 5, DropTracker(&drop_counter)));
            assert_eq!(arena.chunks.lock().unwrap().rest.len(), 1);

            node = arena.alloc(Node(Some(node), 6, DropTracker(&drop_counter)));
            assert_eq!(arena.chunks.lock().unwrap().rest.len(), 1);

            node = arena.alloc(Node(Some(node), 7, DropTracker(&drop_counter)));
            assert_eq!(arena.chunks.lock().unwrap().rest.len(), 2);

            assert_eq!(drop_counter.get(), 0);

            assert_eq!(node.1, 7);
            assert_eq!(node.0.unwrap().1, 6);
            assert_eq!(node.0.unwrap().0.unwrap().1, 5);
            assert!(node.0.unwrap().0.unwrap().0.is_none());

            assert_eq!(drop_counter.get(), 0);
        }
        assert_eq!(drop_counter.get(), 7);
    }

    #[test]
    fn ensure_into_vec_maintains_order_of_allocation() {
        let arena = Arena::with_capacity(1); // force multiple inner vecs
        for &s in &["t", "e", "s", "t"] {
            arena.alloc(String::from(s));
        }
        let vec = arena.into_vec();
        assert_eq!(vec, vec!["t", "e", "s", "t"]);
    }

    #[test]
    fn test_zero_cap() {
        let arena = Arena::with_capacity(0);
        let a = arena.alloc(1);
        let b = arena.alloc(2);
        assert_eq!(*a, 1);
        assert_eq!(*b, 2);
        assert_eq!(arena.len(), 2);
    }

    #[test]
    fn test_alloc_extend() {
        let arena = Arena::with_capacity(2);
        for i in 0..15 {
            let slice = arena.alloc_extend(0..i);
            for (j, &elem) in slice.iter().enumerate() {
                assert_eq!(j, elem);
            }
        }
    }

    #[test]
    fn test_alloc_extend_with_drop_counter() {
        let drop_counter = Cell::new(0);
        {
            let arena = Arena::with_capacity(2);
            let iter = (0..100).map(|j| Node(None, j as u32, DropTracker(&drop_counter)));
            let older_ref = Some(&arena.alloc_extend(iter)[0]);
            assert_eq!(drop_counter.get(), 0);
            let iter = (0..100).map(|j| Node(older_ref, j as u32, DropTracker(&drop_counter)));
            arena.alloc_extend(iter);
            assert_eq!(drop_counter.get(), 0);
        }
        assert_eq!(drop_counter.get(), 200);
    }

    /// Check nothing bad happens by panicking during initialization of borrowed slice.
    #[test]
    fn alloc_uninitialized_with_panic() {
        struct Dropper(bool);

        impl Drop for Dropper {
            fn drop(&mut self) {
                // Just make sure we touch the value, to make sure miri would bite if it was
                // unitialized
                if self.0 {
                    panic!();
                }
            }
        }
        let mut reached_first_init = false;
        panic::catch_unwind(AssertUnwindSafe(|| unsafe {
            let arena: Arena<Dropper> = Arena::new();
            arena.reserve_extend(2);
            let uninitialized = arena.uninitialized_array();
            assert!((*uninitialized).len() >= 2);
            ptr::write((*uninitialized)[0].as_mut_ptr(), Dropper(false));
            reached_first_init = true;
            panic!("To drop the arena");
            // If it didn't panic, we would continue by initializing the second one and confirming by
            // .alloc_uninitialized();
        }))
        .unwrap_err();
        assert!(reached_first_init);
    }

    #[test]
    fn test_uninitialized_array() {
        let arena = Arena::with_capacity(2);
        let uninit = arena.uninitialized_array();
        arena.alloc_extend(0..2);
        unsafe {
            for (&a, b) in (*uninit).iter().zip(0..2) {
                assert_eq!(a.assume_init(), b);
            }
            assert!((*arena.uninitialized_array()).as_ptr() != (*uninit).as_ptr());
            arena.alloc(0);
            let uninit = arena.uninitialized_array();
            assert_eq!((*uninit).len(), 3);
        }
    }

    #[test]
    fn dont_trust_the_iterator_size() {
        use std::iter::repeat_n;

        struct WrongSizeIter<I>(I);
        impl<I> Iterator for WrongSizeIter<I>
        where
            I: Iterator,
        {
            type Item = I::Item;

            fn next(&mut self) -> Option<Self::Item> {
                self.0.next()
            }

            fn size_hint(&self) -> (usize, Option<usize>) {
                (0, Some(0))
            }
        }

        impl<I> ExactSizeIterator for WrongSizeIter<I> where I: Iterator {}

        let arena = Arena::with_capacity(2);
        arena.alloc(0);
        let slice = arena.alloc_extend(WrongSizeIter(repeat_n(1, 1_000)));
        // Allocation of 1000 elements should have created a new chunk
        assert_eq!(arena.chunks.lock().unwrap().rest.len(), 1);
        assert_eq!(slice.len(), 1000);
    }

    #[test]
    fn arena_is_send() {
        fn assert_is_send<T: Send>(_: T) {}

        // If `T` is `Send`, ...
        assert_is_send(42_u32);

        // Then `Arena<T>` is also `Send`.
        let arena: Arena<u32> = Arena::new();
        assert_is_send(arena);
    }

    #[test]
    fn iter_mut_low_capacity() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);

        const MAX: usize = 1_000;
        const CAP: usize = 16;

        let mut arena = Arena::with_capacity(CAP);
        for i in 1..MAX {
            arena.alloc(NonCopy(i));
        }

        assert!(
            arena.chunks.lock().unwrap().rest.len() > 1,
            "expected multiple chunks"
        );

        let mut iter = arena.iter_mut();
        for i in 1..MAX {
            assert_eq!(Some(&mut NonCopy(i)), iter.next());
        }

        assert_eq!(None, iter.next());
    }

    #[test]
    fn iter_mut_high_capacity() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);

        const MAX: usize = 1_000;
        const CAP: usize = 8192;

        let mut arena = Arena::with_capacity(CAP);
        for i in 1..MAX {
            arena.alloc(NonCopy(i));
        }

        assert!(
            arena.chunks.lock().unwrap().rest.is_empty(),
            "expected single chunk"
        );

        let mut iter = arena.iter_mut();
        for i in 1..MAX {
            assert_eq!(Some(&mut NonCopy(i)), iter.next());
        }

        assert_eq!(None, iter.next());
    }

    fn assert_size_hint<T>(arena_len: usize, iter: IterMut<'_, T>) {
        let (min, max) = iter.size_hint();

        assert!(max.is_some());
        let max = max.unwrap();

        // Check that the actual arena length lies between the estimated min and max
        assert!(min <= arena_len);
        assert!(max >= arena_len);

        // Check that the min and max estimates are within a factor of 3
        assert!(min >= arena_len / 3);
        assert!(max <= arena_len * 3);
    }

    #[test]
    fn size_hint() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);

        const MAX: usize = 32;
        const CAP: usize = 0;

        for cap in CAP..(CAP + 16/* check some non-power-of-two capacities */) {
            let mut arena = Arena::with_capacity(cap);
            for i in 1..MAX {
                arena.alloc(NonCopy(i));
                let iter = arena.iter_mut();
                assert_size_hint(i, iter);
            }
        }
    }

    // Ensure that `alloc_extend` doesn't violate provenance of
    // existing references. (Note: This test is pointless except
    // under miri).
    #[test]
    fn check_extend_provenance() {
        let arena = Arena::new();
        let a = arena.alloc(0);
        arena.alloc_extend(core::iter::once(1));
        *a = 1;
    }

    #[test]
    fn size_hint_low_initial_capacities() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);
        #[cfg(miri)]
        const MAX: usize = 100;
        #[cfg(not(miri))]
        const MAX: usize = 25_000;
        const CAP: usize = 0;

        for cap in CAP..(CAP + 128/* check some non-power-of-two capacities */) {
            let mut arena = Arena::with_capacity(cap);
            for i in 1..MAX {
                arena.alloc(NonCopy(i));
                let iter = arena.iter_mut();
                assert_size_hint(i, iter);
            }
        }
    }

    #[test]
    fn size_hint_high_initial_capacities() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);

        #[cfg(miri)]
        const MAX: usize = 100;
        #[cfg(not(miri))]
        const MAX: usize = 25_000;
        const CAP: usize = 8164;

        for cap in CAP..(CAP + 128/* check some non-power-of-two capacities */) {
            let mut arena = Arena::with_capacity(cap);
            for i in 1..MAX {
                arena.alloc(NonCopy(i));
                let iter = arena.iter_mut();
                assert_size_hint(i, iter);
            }
        }
    }

    #[test]
    fn size_hint_many_items() {
        #[derive(Debug, PartialEq, Eq)]
        struct NonCopy(usize);

        #[cfg(miri)]
        const MAX: usize = 500;
        #[cfg(not(miri))]
        const MAX: usize = 5_000_000;
        const CAP: usize = 16;

        let mut arena = Arena::with_capacity(CAP);
        for i in 1..MAX {
            arena.alloc(NonCopy(i));
            let iter = arena.iter_mut();
            assert_size_hint(i, iter);
        }
    }
}
