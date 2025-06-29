use std::sync::{LazyLock, Mutex};

use bstr::{BStr, ByteSlice};
use indexmap::IndexSet;
use rustc_hash::FxBuildHasher;
use strum::EnumCount;

use crate::arena::Arena;

use super::token::Kw;

const INTERNER_ARENA_SIZE: usize = 1024 * 1024; // 1MB

const KEYWORDS: [(&[u8], Kw); Kw::COUNT] = [
    (b"auto", Kw::Auto),
    (b"extrn", Kw::Extrn),
    (b"case", Kw::Case),
    (b"if", Kw::If),
    (b"else", Kw::Else),
    (b"while", Kw::While),
    (b"switch", Kw::Switch),
    (b"goto", Kw::Goto),
    (b"return", Kw::Return),
];

static INTERNER: LazyLock<StringInterner> = LazyLock::new(|| {
    StringInterner::new(
        INTERNER_ARENA_SIZE,
        KEYWORDS.iter().map(|(s, _)| BStr::new(s)),
    )
});

type FxIndexSet<T> = IndexSet<T, FxBuildHasher>;
pub struct StringInterner {
    map: Mutex<FxIndexSet<&'static BStr>>,
    arena: Arena<u8>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct InternedStr(usize);

impl StringInterner {
    pub fn new(arena_size: usize, initial: impl Iterator<Item = &'static BStr>) -> Self {
        let map = FxIndexSet::from_iter(initial);
        let arena = Arena::with_capacity(arena_size);

        StringInterner {
            map: Mutex::new(map),
            arena,
        }
    }

    pub fn intern(&self, string: &BStr) -> InternedStr {
        let mut map = self.map.lock().unwrap();
        let index = map.get_index_of(string).unwrap_or_else(|| {
            let interned = self.arena.alloc_extend(string.bytes()) as *mut [u8];

            // SAFETY: extends lifetime to static, this is safe because arena is never
            // deallocated while map exists
            map.insert_full(BStr::new(unsafe { &*interned })).0
        });
        InternedStr(index)
    }

    pub fn get_str(&self, interned: InternedStr) -> &BStr {
        let map = self.map.lock().unwrap();
        map.get_index(interned.0)
            .copied()
            .unwrap_or(BStr::new(b"dummy"))
    }
}

impl InternedStr {
    pub fn new(str: &[u8]) -> Self {
        INTERNER.intern(BStr::new(str))
    }

    pub fn dummy() -> Self {
        Self(usize::MAX)
    }

    pub fn to_keyword(self) -> Result<Kw, Self> {
        match KEYWORDS.get(self.0) {
            Some((_, kw)) => Ok(*kw),
            None => Err(self),
        }
    }

    pub fn index(&self) -> usize {
        self.0
    }

    pub fn display(&self) -> &BStr {
        INTERNER.get_str(*self)
    }
}
