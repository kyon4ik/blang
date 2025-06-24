use std::collections::HashMap;
use std::slice;
use std::sync::Mutex;

use crate::arena::Arena;

// TODO: use faster hash
// TODO: maybe use indexed hashmap?
pub struct StringInterner {
    map: Mutex<HashMap<&'static [u8], usize>>,
    arena: Arena<u8>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct InternedStr(usize, usize);

impl StringInterner {
    pub fn new(arena_size: usize) -> Self {
        StringInterner {
            map: Mutex::new(HashMap::new()),
            arena: Arena::with_capacity(arena_size),
        }
    }

    pub fn intern(&self, string: &[u8]) -> InternedStr {
        let mut map = self.map.lock().unwrap();
        InternedStr(
            map.get(string).copied().unwrap_or_else(|| {
                let interned = self.arena.alloc_extend(string.iter().copied()) as *const [u8];
                let id = interned as *const u8 as usize;

                // SAFETY: extends lifetime to static, this is safe because arena is never
                // deallocated while map exists
                map.insert(unsafe { &*interned }, id);
                id
            }),
            string.len(),
        )
    }

    pub fn get_string(&self, id: InternedStr) -> &[u8] {
        // SAFETY: this is safe because `intern` creates valid pointer and size
        unsafe { slice::from_raw_parts(id.0 as *const u8, id.1) }
    }
}

impl InternedStr {
    pub fn index(&self) -> usize {
        self.0
    }
}
