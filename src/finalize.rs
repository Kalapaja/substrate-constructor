use num_bigint::{BigInt, BigUint};
use parity_scale_codec::{Compact, Encode};
use primitive_types::H256;
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use substrate_parser::additional_types::{
    AccountId32, Era, PublicEcdsa, PublicEd25519, PublicSr25519, SignatureEcdsa, SignatureEd25519,
    SignatureSr25519,
};

use crate::fill_prepare::{
    ArrayRegularToFill, ArrayU8ToFill, BitSequenceContent, EraToFill, PrimitiveToFill,
    RegularPrimitiveToFill, SequenceU8ToFill, SpecialTypeToFill, TypeContentToFill, TypeToFill,
    VariantSelector,
};
use crate::traits::Unsigned;

pub trait Finalize {
    type FinalForm: Encode;
    fn finalize(&self) -> Option<Self::FinalForm>;
}

#[derive(Clone, Debug)]
pub enum RegularPrimitive {
    Bool(bool),
    Char(char),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    I256(BigInt),
    Str(String),
    U256(BigUint),
}

impl Encode for RegularPrimitive {
    fn encode(&self) -> Vec<u8> {
        match &self {
            RegularPrimitive::Bool(a) => a.encode(),
            RegularPrimitive::Char(a) => u32::from(*a).encode(),
            RegularPrimitive::I8(a) => a.encode(),
            RegularPrimitive::I16(a) => a.encode(),
            RegularPrimitive::I32(a) => a.encode(),
            RegularPrimitive::I64(a) => a.encode(),
            RegularPrimitive::I128(a) => a.encode(),
            RegularPrimitive::I256(a) => a.to_signed_bytes_le(),
            RegularPrimitive::Str(a) => a.encode(),
            RegularPrimitive::U256(a) => a.to_bytes_le(),
        }
    }
}

impl Finalize for RegularPrimitiveToFill {
    type FinalForm = RegularPrimitive;
    fn finalize(&self) -> Option<Self::FinalForm> {
        match &self {
            RegularPrimitiveToFill::Bool(a) => a.map(RegularPrimitive::Bool),
            RegularPrimitiveToFill::Char(a) => a.map(RegularPrimitive::Char),
            RegularPrimitiveToFill::I8(a) => a.map(RegularPrimitive::I8),
            RegularPrimitiveToFill::I16(a) => a.map(RegularPrimitive::I16),
            RegularPrimitiveToFill::I32(a) => a.map(RegularPrimitive::I32),
            RegularPrimitiveToFill::I64(a) => a.map(RegularPrimitive::I64),
            RegularPrimitiveToFill::I128(a) => a.map(RegularPrimitive::I128),
            RegularPrimitiveToFill::I256(a) => a.clone().map(RegularPrimitive::I256),
            RegularPrimitiveToFill::Str(a) => Some(RegularPrimitive::Str(a.to_owned())),
            RegularPrimitiveToFill::U256(a) => a.clone().map(RegularPrimitive::U256),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Primitive {
    CompactUnsigned(Unsigned),
    Regular(RegularPrimitive),
    Unsigned(Unsigned),
}

impl Encode for Primitive {
    fn encode(&self) -> Vec<u8> {
        match &self {
            Primitive::CompactUnsigned(unsigned) => match unsigned {
                Unsigned::U8(a) => Compact(*a).encode(),
                Unsigned::U16(a) => Compact(*a).encode(),
                Unsigned::U32(a) => Compact(*a).encode(),
                Unsigned::U64(a) => Compact(*a).encode(),
                Unsigned::U128(a) => Compact(*a).encode(),
            },
            Primitive::Regular(regular_primitive) => regular_primitive.encode(),
            Primitive::Unsigned(unsigned) => match unsigned {
                Unsigned::U8(a) => a.encode(),
                Unsigned::U16(a) => a.encode(),
                Unsigned::U32(a) => a.encode(),
                Unsigned::U64(a) => a.encode(),
                Unsigned::U128(a) => a.encode(),
            },
        }
    }
}

impl Finalize for PrimitiveToFill {
    type FinalForm = Primitive;
    fn finalize(&self) -> Option<Self::FinalForm> {
        match &self {
            PrimitiveToFill::CompactUnsigned(unsigned_to_fill) => unsigned_to_fill
                .content
                .into_unsigned()
                .map(Primitive::CompactUnsigned),
            PrimitiveToFill::Regular(regular_primitive) => {
                regular_primitive.finalize().map(Primitive::Regular)
            }
            PrimitiveToFill::Unsigned(unsigned_to_fill) => unsigned_to_fill
                .content
                .into_unsigned()
                .map(Primitive::Unsigned),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ArrayU8(pub Vec<u8>);

impl Encode for ArrayU8 {
    fn encode(&self) -> Vec<u8> {
        self.0.to_owned()
    }
}

impl Finalize for ArrayU8ToFill {
    type FinalForm = ArrayU8;
    fn finalize(&self) -> Option<Self::FinalForm> {
        if self.content.len() as u32 == self.len {
            Some(ArrayU8(self.content.to_owned()))
        } else {
            None
        }
    }
}

#[derive(Clone, Debug)]
pub struct ArrayRegular(pub Vec<TypeContent>);

impl Encode for ArrayRegular {
    fn encode(&self) -> Vec<u8> {
        let mut out: Vec<u8> = Vec::new();
        for element in self.0.iter() {
            out.extend_from_slice(&element.encode())
        }
        out
    }
}

impl Finalize for ArrayRegularToFill {
    type FinalForm = ArrayRegular;
    fn finalize(&self) -> Option<Self::FinalForm> {
        if self.content.len() as u32 == self.len {
            let mut array_regular = ArrayRegular(Vec::new());
            for element in self.content.iter() {
                match element.finalize() {
                    Some(a) => array_regular.0.push(a.to_owned()),
                    None => return None,
                }
            }
            Some(array_regular)
        } else {
            None
        }
    }
}

impl Finalize for SequenceU8ToFill {
    type FinalForm = Vec<u8>;
    fn finalize(&self) -> Option<Self::FinalForm> {
        Some(self.content.to_owned())
    }
}

#[derive(Clone, Debug)]
pub enum SpecialType {
    AccountId32(AccountId32),
    Era(Era),
    H256(H256),
    PerU16 {
        value: PerU16,
        is_compact: bool,
    },
    Perbill {
        value: Perbill,
        is_compact: bool,
    },
    Percent {
        value: Percent,
        is_compact: bool,
    },
    Permill {
        value: Permill,
        is_compact: bool,
    },
    Perquintill {
        value: Perquintill,
        is_compact: bool,
    },
    PublicEd25519(PublicEd25519),
    PublicSr25519(PublicSr25519),
    PublicEcdsa(PublicEcdsa),
    SignatureEd25519(SignatureEd25519),
    SignatureSr25519(SignatureSr25519),
    SignatureEcdsa(SignatureEcdsa),
}

impl Encode for SpecialType {
    fn encode(&self) -> Vec<u8> {
        match &self {
            SpecialType::AccountId32(a) => a.0.to_vec(),
            SpecialType::Era(a) => a.encode(),
            SpecialType::H256(a) => a.0.to_vec(),
            SpecialType::PerU16 { value, is_compact } => {
                if *is_compact {
                    Compact(*value).encode()
                } else {
                    value.encode()
                }
            }
            SpecialType::Perbill { value, is_compact } => {
                if *is_compact {
                    Compact(*value).encode()
                } else {
                    value.encode()
                }
            }
            SpecialType::Percent { value, is_compact } => {
                if *is_compact {
                    Compact(*value).encode()
                } else {
                    value.encode()
                }
            }
            SpecialType::Permill { value, is_compact } => {
                if *is_compact {
                    Compact(*value).encode()
                } else {
                    value.encode()
                }
            }
            SpecialType::Perquintill { value, is_compact } => {
                if *is_compact {
                    Compact(*value).encode()
                } else {
                    value.encode()
                }
            }
            SpecialType::PublicEd25519(a) => a.0.to_vec(),
            SpecialType::PublicSr25519(a) => a.0.to_vec(),
            SpecialType::PublicEcdsa(a) => a.0.to_vec(),
            SpecialType::SignatureEd25519(a) => a.0.to_vec(),
            SpecialType::SignatureSr25519(a) => a.0.to_vec(),
            SpecialType::SignatureEcdsa(a) => a.0.to_vec(),
        }
    }
}

macro_rules! finalize_perthing_compact {
    ($value:tt, $is_compact:tt, $variant:ident) => {
        $value.as_ref().map(|a| SpecialType::$variant {
            value: *a,
            is_compact: *$is_compact,
        })
    };
}

impl Finalize for SpecialTypeToFill {
    type FinalForm = SpecialType;
    fn finalize(&self) -> Option<Self::FinalForm> {
        match &self {
            SpecialTypeToFill::AccountId32(a) => a.clone().map(SpecialType::AccountId32),
            SpecialTypeToFill::Era(a) => match a {
                EraToFill::Immortal => Some(SpecialType::Era(Era::Immortal)),
                EraToFill::Mortal {
                    period: optional_period,
                    phase: optional_phase,
                } => {
                    if let Some(period) = optional_period {
                        optional_phase
                            .as_ref()
                            .map(|phase| SpecialType::Era(Era::Mortal(*period, *phase)))
                    } else {
                        None
                    }
                }
            },
            SpecialTypeToFill::H256(a) => a.hash.map(SpecialType::H256),
            SpecialTypeToFill::PerU16 { value, is_compact } => {
                finalize_perthing_compact!(value, is_compact, PerU16)
            }
            SpecialTypeToFill::Perbill { value, is_compact } => {
                finalize_perthing_compact!(value, is_compact, Perbill)
            }
            SpecialTypeToFill::Percent { value, is_compact } => {
                finalize_perthing_compact!(value, is_compact, Percent)
            }
            SpecialTypeToFill::Permill { value, is_compact } => {
                finalize_perthing_compact!(value, is_compact, Permill)
            }
            SpecialTypeToFill::Perquintill { value, is_compact } => {
                finalize_perthing_compact!(value, is_compact, Perquintill)
            }
            SpecialTypeToFill::PublicEd25519(a) => a.clone().map(SpecialType::PublicEd25519),
            SpecialTypeToFill::PublicSr25519(a) => a.clone().map(SpecialType::PublicSr25519),
            SpecialTypeToFill::PublicEcdsa(a) => a.clone().map(SpecialType::PublicEcdsa),
            SpecialTypeToFill::SignatureEd25519(a) => a.clone().map(SpecialType::SignatureEd25519),
            SpecialTypeToFill::SignatureSr25519(a) => a.clone().map(SpecialType::SignatureSr25519),
            SpecialTypeToFill::SignatureEcdsa(a) => a.clone().map(SpecialType::SignatureEcdsa),
        }
    }
}

#[derive(Clone, Debug)]
pub struct VariantContent {
    pub index: u8,
    pub fields_content: Vec<TypeContent>,
}

impl Encode for VariantContent {
    fn encode(&self) -> Vec<u8> {
        let mut out = vec![self.index];
        for field in self.fields_content.iter() {
            out.extend_from_slice(&field.encode())
        }
        out
    }
}

impl Finalize for VariantSelector {
    type FinalForm = VariantContent;
    fn finalize(&self) -> Option<Self::FinalForm> {
        let mut fields_content = Vec::new();
        for field in self.selected.fields_to_fill.iter() {
            match field.type_to_fill.finalize() {
                Some(a) => fields_content.push(a.to_owned()),
                None => return None,
            }
        }
        Some(VariantContent {
            index: self.selected.index,
            fields_content,
        })
    }
}

impl Encode for BitSequenceContent {
    fn encode(&self) -> Vec<u8> {
        match &self {
            BitSequenceContent::BitVecU8Lsb0(a) => a.encode(),
            BitSequenceContent::BitVecU16Lsb0(a) => a.encode(),
            BitSequenceContent::BitVecU32Lsb0(a) => a.encode(),
            #[cfg(target_pointer_width = "64")]
            BitSequenceContent::BitVecU64Lsb0(a) => a.encode(),
            BitSequenceContent::BitVecU8Msb0(a) => a.encode(),
            BitSequenceContent::BitVecU16Msb0(a) => a.encode(),
            BitSequenceContent::BitVecU32Msb0(a) => a.encode(),
            #[cfg(target_pointer_width = "64")]
            BitSequenceContent::BitVecU64Msb0(a) => a.encode(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum TypeContent {
    ArrayU8(ArrayU8),
    ArrayRegular(ArrayRegular),
    BitSequence(BitSequenceContent),
    Composite(Vec<TypeContent>),
    Primitive(Primitive),
    SequenceRegular(Vec<TypeContent>),
    SequenceU8(Vec<u8>),
    SpecialType(SpecialType),
    Tuple(Vec<TypeContent>),
    Variant(VariantContent),
    VariantEmpty,
}

impl Encode for TypeContent {
    fn encode(&self) -> Vec<u8> {
        match &self {
            TypeContent::ArrayU8(a) => a.encode(),
            TypeContent::ArrayRegular(a) => a.encode(),
            TypeContent::BitSequence(a) => a.encode(),
            TypeContent::Composite(composite_fields) => {
                let mut out: Vec<u8> = Vec::new();
                for element in composite_fields.iter() {
                    out.extend_from_slice(&element.encode())
                }
                out
            }
            TypeContent::Primitive(a) => a.encode(),
            TypeContent::SequenceRegular(a) => a.encode(),
            TypeContent::SequenceU8(a) => a.encode(),
            TypeContent::SpecialType(a) => a.encode(),
            TypeContent::Tuple(tuple_fields) => {
                let mut out: Vec<u8> = Vec::new();
                for element in tuple_fields.iter() {
                    out.extend_from_slice(&element.encode())
                }
                out
            }
            TypeContent::Variant(a) => a.encode(),
            TypeContent::VariantEmpty => Vec::new(),
        }
    }
}

impl Finalize for TypeContentToFill {
    type FinalForm = TypeContent;
    fn finalize(&self) -> Option<Self::FinalForm> {
        match &self {
            TypeContentToFill::ArrayU8(array_u8_to_fill) => {
                array_u8_to_fill.finalize().map(TypeContent::ArrayU8)
            }
            TypeContentToFill::ArrayRegular(array_regular_to_fill) => array_regular_to_fill
                .finalize()
                .map(TypeContent::ArrayRegular),
            TypeContentToFill::BitSequence(bit_sequence_content) => {
                Some(TypeContent::BitSequence(bit_sequence_content.to_owned()))
            }
            TypeContentToFill::Composite(fields_set) => {
                let mut composite_fields = Vec::new();
                for element in fields_set.iter() {
                    match element.type_to_fill.content.finalize() {
                        Some(a) => composite_fields.push(a),
                        None => return None,
                    }
                }
                Some(TypeContent::Composite(composite_fields))
            }
            TypeContentToFill::Primitive(primitive_to_fill) => {
                primitive_to_fill.finalize().map(TypeContent::Primitive)
            }
            TypeContentToFill::SequenceU8(sequence_u8_to_fill) => Some(TypeContent::SequenceU8(
                sequence_u8_to_fill.content.to_owned(),
            )),
            TypeContentToFill::SequenceRegular(sequence_regular_to_fill) => {
                let mut sequence = Vec::new();
                for element in sequence_regular_to_fill.content.iter() {
                    match element.finalize() {
                        Some(a) => sequence.push(a),
                        None => return None,
                    }
                }
                Some(TypeContent::SequenceRegular(sequence))
            }
            TypeContentToFill::SpecialType(special_type_to_fill) => special_type_to_fill
                .finalize()
                .map(TypeContent::SpecialType),
            TypeContentToFill::Tuple(fields_set) => {
                let mut tuple_fields = Vec::new();
                for element in fields_set.iter() {
                    match element.content.finalize() {
                        Some(a) => tuple_fields.push(a),
                        None => return None,
                    }
                }
                Some(TypeContent::Tuple(tuple_fields))
            }
            TypeContentToFill::Variant(variant_selector) => {
                variant_selector.finalize().map(TypeContent::Variant)
            }
            TypeContentToFill::VariantEmpty => Some(TypeContent::VariantEmpty),
        }
    }
}

impl Finalize for TypeToFill {
    type FinalForm = TypeContent;
    fn finalize(&self) -> Option<Self::FinalForm> {
        self.content.finalize()
    }
}
