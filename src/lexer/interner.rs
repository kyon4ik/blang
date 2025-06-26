use std::collections::HashMap;
use std::slice;
use std::sync::Mutex;

use bstr::{BStr, ByteSlice};

use crate::arena::Arena;

// TODO: use faster hash
// TODO: maybe use indexed hashmap?
pub struct StringInterner {
    map: Mutex<HashMap<&'static BStr, usize>>,
    arena: Arena<u8>,
}

// FIXME: This maybe unsafe (as internered is not bounded to concrete interner or its lifetime)
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct InternedStr(usize, usize);

impl StringInterner {
    pub fn new(arena_size: usize) -> Self {
        StringInterner {
            map: Mutex::new(HashMap::new()),
            arena: Arena::with_capacity(arena_size),
        }
    }

    pub fn intern(&self, string: &BStr) -> InternedStr {
        let mut map = self.map.lock().unwrap();
        InternedStr(
            map.get(string).copied().unwrap_or_else(|| {
                let interned = self.arena.alloc_extend(string.bytes()) as *const [u8];
                let id = interned as *const u8 as usize;

                // SAFETY: extends lifetime to static, this is safe because arena is never
                // deallocated while map exists
                map.insert(BStr::new(unsafe { &*interned }), id);
                id
            }),
            string.len(),
        )
    }

    pub fn get_string(&self, id: InternedStr) -> &BStr {
        BStr::new(unsafe { slice::from_raw_parts(id.0 as *const u8, id.1) })
    }
}

impl InternedStr {
    pub fn index(&self) -> usize {
        self.0
    }
}
