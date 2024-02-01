use bitvec::prelude::{BitVec, Lsb0, Msb0};
use num_bigint::{BigInt, BigUint};

use crate::fill_prepare::{TypeContentToFill, TypeToFill};

pub enum ActionCard {
    SelectIndex,
    
}

pub trait CheckFirstEmpty {
    fn first_empty(&self) -> Option<FirstEmpty>;
}

impl CheckFirstEmpty for TypeToFill {
    fn first_empty(&self) -> Option<FirstEmpty> {
        match &self.content {
            TypeContentToFill::Array(array_to_fill) => {},
            TypeContentToFill::BitSequence(bit_sequence_to_fill) => {},
            TypeContentToFill::Composite(fields_to_fill) => {}
            TypeContentToFill::Primitive(primitive_to_fill) => {},
            TypeContentToFill::Sequence(sequence_to_fill) => {},
            TypeContentToFill::SpecialType(special_type_to_fill) => {},
            TypeContentToFill::Tuple(types_to_fill) => {}
            TypeContentToFill::Variant(variant_selector) => {},
        }
        None
    }
}

pub struct FirstEmpty {
    pub type_to_fill: TypeToFill,
    pub requested_entry: RequestedEntry,
}

#[derive(Debug)]
pub enum RequestedEntry {
    VariantIndex,
    BitVecU8Lsb0,
    BitVecU16Lsb0,
    BitVecU32Lsb0,
    #[cfg(target_pointer_width = "64")]
    BitVecU64Lsb0,
    BitVecU8Msb0,
    BitVecU16Msb0,
    BitVecU32Msb0,
    #[cfg(target_pointer_width = "64")]
    BitVecU64Msb0,
    Bool,
    Char,
    Str,
    I8,
    I16,
    I32,
    I64,
    I128,
    I256,
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
    VecU8,
}

#[derive(Debug)]
pub enum ReceivedEntry {
    VariantIndex(u8),
    BitVecU8Lsb0(BitVec<u8, Lsb0>),
    BitVecU16Lsb0(BitVec<u16, Lsb0>),
    BitVecU32Lsb0(BitVec<u32, Lsb0>),
    #[cfg(target_pointer_width = "64")]
    BitVecU64Lsb0(BitVec<u64, Lsb0>),
    BitVecU8Msb0(BitVec<u8, Msb0>),
    BitVecU16Msb0(BitVec<u16, Msb0>),
    BitVecU32Msb0(BitVec<u32, Msb0>),
    #[cfg(target_pointer_width = "64")]
    BitVecU64Msb0(BitVec<u64, Msb0>),
    Bool(bool),
    Char(char),
    Str(String),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    I256(BigInt),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    U256(BigUint),
    VecU8(Vec<u8>),
}
