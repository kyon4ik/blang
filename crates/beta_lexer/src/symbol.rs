use crate::ascii::AChar;

pub struct Interner {
    // index_map: HashMap<u64, usize>,
    // values: Vec<(u64, &'static AChar, usize)>,
    arena: Vec<AChar>,
}
