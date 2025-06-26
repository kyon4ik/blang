use std::collections::HashMap;
use std::slice;
use std::sync::{LazyLock, Mutex};

use bstr::{BStr, ByteSlice};

use crate::arena::Arena;

const INTERNER_ARENA_SIZE: usize = 1024 * 1024; // 1MB

static INTERNER: LazyLock<StringInterner> =
    LazyLock::new(|| StringInterner::new(INTERNER_ARENA_SIZE));

// TODO: use faster hash
// TODO: maybe use indexed hashmap?
// NOTE: do NOT create more than one interner (as internered is not bounded to concrete interner or its lifetime)
pub struct StringInterner {
    map: Mutex<HashMap<&'static BStr, usize>>,
    arena: Arena<u8>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct InternedStr(usize, usize);

impl StringInterner {
    fn new(arena_size: usize) -> Self {
        StringInterner {
            map: Mutex::new(HashMap::new()),
            arena: Arena::with_capacity(arena_size),
        }
    }

    fn intern(&self, string: &BStr) -> InternedStr {
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
}

impl InternedStr {
    pub fn new(str: &[u8]) -> Self {
        INTERNER.intern(BStr::new(str))
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn display(&self) -> &BStr {
        // SAFETY: There exist only one StringInterner
        BStr::new(unsafe { slice::from_raw_parts(self.0 as *const u8, self.1) })
    }
}
