use std::str::FromStr;

use num_bigint::{BigInt, BigUint};
use primitive_types::H256;
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use substrate_crypto_light::{
    common::AccountId32,
    ecdsa::{Public as PublicEcdsa, Signature as SignatureEcdsa},
    ed25519::{Public as PublicEd25519, Signature as SignatureEd25519},
    sr25519::{Public as PublicSr25519, Signature as SignatureSr25519},
};

use crate::{
    fill_prepare::{
        ArrayU8ToFill, EraToFill, RegularPrimitiveToFill, SequenceU8ToFill, SpecialTypeToFill,
        UnsignedToFill,
    },
    traits::Unsigned,
};

pub trait TryFill {
    fn upd_from_str(&mut self, source: &str);
}

macro_rules! primitive_str {
    ($ty:ty, $old:tt, $source:tt) => {
        if let Ok(value) = <$ty>::from_str($source) {
            *$old = Some(value)
        }
    };
}

impl TryFill for UnsignedToFill {
    fn upd_from_str(&mut self, source: &str) {
        match self {
            UnsignedToFill::U8(ref mut old) => primitive_str!(u8, old, source),
            UnsignedToFill::U16(ref mut old) => primitive_str!(u16, old, source),
            UnsignedToFill::U32(ref mut old) => primitive_str!(u32, old, source),
            UnsignedToFill::U64(ref mut old) => primitive_str!(u64, old, source),
            UnsignedToFill::U128(ref mut old) => primitive_str!(u128, old, source),
        }
    }
}

macro_rules! unsigned {
    ($variant:ident, $old:tt, $source:tt) => {
        if let Unsigned::$variant(value) = $source {
            *$old = Some(*value)
        }
    };
}

macro_rules! rpc_u32_fit {
    ($ty:ty, $old:tt, $source:tt) => {
        if let Ok(value) = <$ty>::try_from($source) {
            *$old = Some(value)
        }
    };
}

impl UnsignedToFill {
    pub fn upd_from_unsigned(&mut self, source: &Unsigned) {
        match self {
            UnsignedToFill::U8(ref mut old) => unsigned!(U8, old, source),
            UnsignedToFill::U16(ref mut old) => unsigned!(U16, old, source),
            UnsignedToFill::U32(ref mut old) => unsigned!(U32, old, source),
            UnsignedToFill::U64(ref mut old) => unsigned!(U64, old, source),
            UnsignedToFill::U128(ref mut old) => unsigned!(U128, old, source),
        }
    }
    #[allow(irrefutable_let_patterns)]
    pub fn upd_from_rpc_u32(&mut self, source: u32) {
        match self {
            UnsignedToFill::U8(ref mut old) => rpc_u32_fit!(u8, old, source),
            UnsignedToFill::U16(ref mut old) => rpc_u32_fit!(u16, old, source),
            UnsignedToFill::U32(ref mut old) => rpc_u32_fit!(u32, old, source),
            UnsignedToFill::U64(ref mut old) => rpc_u32_fit!(u64, old, source),
            UnsignedToFill::U128(ref mut old) => rpc_u32_fit!(u128, old, source),
        }
    }
}

impl TryFill for RegularPrimitiveToFill {
    fn upd_from_str(&mut self, source: &str) {
        match self {
            RegularPrimitiveToFill::Bool(ref mut old) => primitive_str!(bool, old, source),
            RegularPrimitiveToFill::Char(ref mut old) => primitive_str!(char, old, source),
            RegularPrimitiveToFill::I8(ref mut old) => primitive_str!(i8, old, source),
            RegularPrimitiveToFill::I16(ref mut old) => primitive_str!(i16, old, source),
            RegularPrimitiveToFill::I32(ref mut old) => primitive_str!(i32, old, source),
            RegularPrimitiveToFill::I64(ref mut old) => primitive_str!(i64, old, source),
            RegularPrimitiveToFill::I128(ref mut old) => primitive_str!(i128, old, source),
            RegularPrimitiveToFill::I256(ref mut old) => primitive_str!(BigInt, old, source),
            RegularPrimitiveToFill::Str(ref mut old) => source.clone_into(old),
            RegularPrimitiveToFill::U256(ref mut old) => primitive_str!(BigUint, old, source),
        }
    }
}

macro_rules! array_from_hex {
    ($ty:expr, $old:tt, $source:tt) => {
        if let Ok(value) = hex::decode($source) {
            if let Ok(array) = value.try_into() {
                *$old = Some($ty(array))
            }
        }
    };
}

macro_rules! perthing_from_str {
    ($ty:ty, $inner_ty:ty, $old:tt, $source:tt) => {
        if let Ok(parts) = <$inner_ty>::from_str($source) {
            *$old = Some(<$ty>::from_parts(parts))
        }
    };
}

impl TryFill for SpecialTypeToFill {
    fn upd_from_str(&mut self, source: &str) {
        match self {
            SpecialTypeToFill::AccountId32(ref mut old) => {
                array_from_hex!(AccountId32, old, source)
            }
            SpecialTypeToFill::Era(ref mut old) => {
                let possibly_mortal: Vec<&str> = source.split(' ').collect();
                if possibly_mortal.len() == 2 {
                    if let Ok(period_entry) = u64::from_str(possibly_mortal[0]) {
                        if let Ok(block_number_entry) = u64::from_str(possibly_mortal[1]) {
                            *old = EraToFill::Mortal {
                                period_entry,
                                block_number_entry: Some(block_number_entry),
                            };
                        }
                    }
                }
            }
            SpecialTypeToFill::H256 {
                hash: ref mut old,
                specialty: _,
            } => array_from_hex!(H256, old, source),
            SpecialTypeToFill::PerU16 {
                value: ref mut old,
                is_compact: _,
            } => perthing_from_str!(PerU16, u16, old, source),
            SpecialTypeToFill::Perbill {
                value: ref mut old,
                is_compact: _,
            } => perthing_from_str!(Perbill, u32, old, source),
            SpecialTypeToFill::Percent {
                value: ref mut old,
                is_compact: _,
            } => perthing_from_str!(Percent, u8, old, source),
            SpecialTypeToFill::Permill {
                value: ref mut old,
                is_compact: _,
            } => perthing_from_str!(Permill, u32, old, source),
            SpecialTypeToFill::Perquintill {
                value: ref mut old,
                is_compact: _,
            } => perthing_from_str!(Perquintill, u64, old, source),
            SpecialTypeToFill::PublicEd25519(ref mut old) => {
                array_from_hex!(PublicEd25519, old, source)
            }
            SpecialTypeToFill::PublicSr25519(ref mut old) => {
                array_from_hex!(PublicSr25519, old, source)
            }
            SpecialTypeToFill::PublicEcdsa(ref mut old) => {
                array_from_hex!(PublicEcdsa, old, source)
            }
            SpecialTypeToFill::SignatureEd25519(ref mut old) => {
                array_from_hex!(SignatureEd25519, old, source)
            }
            SpecialTypeToFill::SignatureSr25519(ref mut old) => {
                array_from_hex!(SignatureSr25519, old, source)
            }
            SpecialTypeToFill::SignatureEcdsa(ref mut old) => {
                array_from_hex!(SignatureEcdsa, old, source)
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
