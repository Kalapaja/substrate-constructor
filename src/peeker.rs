use crate::fill_prepare::{TypeContentToFill, TypeToFill};

pub enum Peeker<'a> {
    Depth(usize),
    Done(&'a TypeToFill),
}

impl<'a> Peeker<'a> {
    /// Extract type at given depth
    pub fn peek(input: &'a TypeToFill, position: usize) -> Self {
        if position == 0 {
            return Peeker::Done(input);
        }
        let mut depth = position - 1;
        if let TypeContentToFill::Variant(ref a) = input.content {
            for i in &a.selected.fields_to_fill {
                match Self::peek(&i.type_to_fill, depth) {
                    Self::Depth(a) => depth = a,
                    Self::Done(a) => return Self::Done(a),
                }
            }
        }
        Self::Depth(depth)
    }
}
