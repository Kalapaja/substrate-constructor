use std::str::FromStr;

use num_bigint::{BigInt, BigUint};

use primitive_types::H256;

use crate::fill_prepare::{
    ArrayU8ToFill, H256ToFill, RegularPrimitiveToFill, SequenceU8ToFill, UnsignedToFill,
};

pub trait TryFill {
    fn upd_from_str(&mut self, source: &str);
}

impl TryFill for UnsignedToFill {
    fn upd_from_str(&mut self, source: &str) {
        match self {
            UnsignedToFill::U8(ref mut old_value) => {
                if let Ok(value) = u8::from_str(source) {
                    *old_value = Some(value)
                }
            }
            UnsignedToFill::U16(ref mut old_value) => {
                if let Ok(value) = u16::from_str(source) {
                    *old_value = Some(value)
                }
            }
            UnsignedToFill::U32(ref mut old_value) => {
                if let Ok(value) = u32::from_str(source) {
                    *old_value = Some(value)
                }
            }
            UnsignedToFill::U64(ref mut old_value) => {
                if let Ok(value) = u64::from_str(source) {
                    *old_value = Some(value)
                }
            }
            UnsignedToFill::U128(ref mut old_value) => {
                if let Ok(value) = u128::from_str(source) {
                    *old_value = Some(value)
                }
            }
        }
    }
}

impl TryFill for RegularPrimitiveToFill {
    fn upd_from_str(&mut self, source: &str) {
        match self {
            RegularPrimitiveToFill::Bool(ref mut old_value) => {
                if let Ok(value) = bool::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::Char(ref mut old_value) => {
                if let Ok(value) = char::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I8(ref mut old_value) => {
                if let Ok(value) = i8::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I16(ref mut old_value) => {
                if let Ok(value) = i16::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I32(ref mut old_value) => {
                if let Ok(value) = i32::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I64(ref mut old_value) => {
                if let Ok(value) = i64::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I128(ref mut old_value) => {
                if let Ok(value) = i128::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::I256(ref mut old_value) => {
                if let Ok(value) = BigInt::from_str(source) {
                    *old_value = Some(value)
                }
            }
            RegularPrimitiveToFill::Str(ref mut old_value) => *old_value = source.to_owned(),
            RegularPrimitiveToFill::U256(ref mut old_value) => {
                if let Ok(value) = BigUint::from_str(source) {
                    *old_value = Some(value)
                }
            }
        }
    }
}

impl TryFill for H256ToFill {
    fn upd_from_str(&mut self, source: &str) {
        if let Ok(value) = hex::decode(source) {
            if let Ok(hash) = value.try_into() {
                self.hash = Some(H256(hash))
            }
        }
    }
}

pub trait TryBytesFill {
    fn upd_from_single_byte(&mut self, source: &str);
    fn upd_from_hex(&mut self, source: &str);
    fn upd_from_utf8(&mut self, source: &str);
}

impl TryBytesFill for ArrayU8ToFill {
    fn upd_from_single_byte(&mut self, source: &str) {
        if let Ok(addition) = u8::from_str(source) {
            if (self.content.len() as u32) < self.len {
                self.content.push(addition)
            }
        }
    }
    fn upd_from_hex(&mut self, source: &str) {
        if let Ok(value) = hex::decode(source) {
            if value.len() as u32 == self.len {
                self.content = value;
            }
        }
    }
    fn upd_from_utf8(&mut self, source: &str) {
        let value = source.as_bytes().to_vec();
        if value.len() as u32 == self.len {
            self.content = value;
        }
    }
}

impl TryBytesFill for SequenceU8ToFill {
    fn upd_from_single_byte(&mut self, source: &str) {
        if let Ok(addition) = u8::from_str(source) {
            self.content.push(addition)
        }
    }
    fn upd_from_hex(&mut self, source: &str) {
        if let Ok(value) = hex::decode(source) {
            self.content = value;
        }
    }
    fn upd_from_utf8(&mut self, source: &str) {
        self.content = source.as_bytes().to_vec();
    }
}
