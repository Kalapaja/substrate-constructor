use crate::fill_prepare::{
    ArrayRegularToFill, ArrayU8ToFill, BitSequenceToFill, EraToFill, FieldToFill, PrimitiveToFill,
    RegularPrimitiveToFill, SequenceRegularToFill, SequenceU8ToFill, SpecialTypeToFill,
    SpecialtyUnsignedToFill, TransactionToFill, TypeContentToFill, UnsignedToFill, VariantSelected,
    VariantSelector,
};

pub trait IsReady {
    fn is_ready(&self) -> bool;
}

impl IsReady for TransactionToFill {
    fn is_ready(&self) -> bool {
        if self.author.content.is_ready() {
            return false;
        }
        if !self.call.content.is_ready() {
            return false;
        }
        for extension in self.extensions.iter() {
            if !extension.content.is_ready() {
                return false;
            }
        }
        true
    }
}

impl IsReady for TypeContentToFill {
    fn is_ready(&self) -> bool {
        match &self {
            TypeContentToFill::ArrayU8(array_u8_to_fill) => array_u8_to_fill.is_ready(),
            TypeContentToFill::ArrayRegular(array_regular_to_fill) => {
                array_regular_to_fill.is_ready()
            }
            TypeContentToFill::BitSequence(bit_sequence_to_fill) => bit_sequence_to_fill.is_ready(),
            TypeContentToFill::Composite(fields_to_fill) => {
                for field in fields_to_fill.iter() {
                    if !field.is_ready() {
                        return false;
                    }
                }
                true
            }
            TypeContentToFill::Primitive(primitive_to_fill) => primitive_to_fill.is_ready(),
            TypeContentToFill::SequenceU8(sequence_u8_to_fill) => sequence_u8_to_fill.is_ready(),
            TypeContentToFill::SequenceRegular(sequence_regular_to_fill) => {
                sequence_regular_to_fill.is_ready()
            }
            TypeContentToFill::SpecialType(special_type_to_fill) => special_type_to_fill.is_ready(),
            TypeContentToFill::Tuple(types_to_fill) => {
                for ty in types_to_fill.iter() {
                    if !ty.content.is_ready() {
                        return false;
                    }
                }
                true
            }
            TypeContentToFill::Variant(variant_selector) => variant_selector.is_ready(),
            TypeContentToFill::VariantEmpty => true,
        }
    }
}

impl IsReady for VariantSelector {
    fn is_ready(&self) -> bool {
        self.selected.is_ready()
    }
}

impl IsReady for FieldToFill {
    fn is_ready(&self) -> bool {
        self.type_to_fill.content.is_ready()
    }
}

impl IsReady for VariantSelected {
    fn is_ready(&self) -> bool {
        for field in self.fields_to_fill.iter() {
            if !field.is_ready() {
                return false;
            }
        }
        true
    }
}

impl IsReady for BitSequenceToFill {
    fn is_ready(&self) -> bool {
        match &self {
            BitSequenceToFill::BitVecU8Lsb0(a) => a.is_some(),
            BitSequenceToFill::BitVecU16Lsb0(a) => a.is_some(),
            BitSequenceToFill::BitVecU32Lsb0(a) => a.is_some(),
            #[cfg(target_pointer_width = "64")]
            BitSequenceToFill::BitVecU64Lsb0(a) => a.is_some(),
            BitSequenceToFill::BitVecU8Msb0(a) => a.is_some(),
            BitSequenceToFill::BitVecU16Msb0(a) => a.is_some(),
            BitSequenceToFill::BitVecU32Msb0(a) => a.is_some(),
            #[cfg(target_pointer_width = "64")]
            BitSequenceToFill::BitVecU64Msb0(a) => a.is_some(),
        }
    }
}

impl IsReady for PrimitiveToFill {
    fn is_ready(&self) -> bool {
        match &self {
            PrimitiveToFill::CompactUnsigned(a) => a.is_ready(),
            PrimitiveToFill::Regular(a) => a.is_ready(),
            PrimitiveToFill::Unsigned(a) => a.is_ready(),
        }
    }
}

impl IsReady for RegularPrimitiveToFill {
    fn is_ready(&self) -> bool {
        match &self {
            RegularPrimitiveToFill::Bool(a) => a.is_some(),
            RegularPrimitiveToFill::Char(a) => a.is_some(),
            RegularPrimitiveToFill::I8(a) => a.is_some(),
            RegularPrimitiveToFill::I16(a) => a.is_some(),
            RegularPrimitiveToFill::I32(a) => a.is_some(),
            RegularPrimitiveToFill::I64(a) => a.is_some(),
            RegularPrimitiveToFill::I128(a) => a.is_some(),
            RegularPrimitiveToFill::I256(a) => a.is_some(),
            RegularPrimitiveToFill::Str(_) => true,
            RegularPrimitiveToFill::U256(a) => a.is_some(),
        }
    }
}

impl IsReady for SpecialtyUnsignedToFill {
    fn is_ready(&self) -> bool {
        match &self.content {
            UnsignedToFill::U8(a) => a.is_some(),
            UnsignedToFill::U16(a) => a.is_some(),
            UnsignedToFill::U32(a) => a.is_some(),
            UnsignedToFill::U64(a) => a.is_some(),
            UnsignedToFill::U128(a) => a.is_some(),
        }
    }
}

impl IsReady for SequenceU8ToFill {
    fn is_ready(&self) -> bool {
        true
    }
}

impl IsReady for SequenceRegularToFill {
    fn is_ready(&self) -> bool {
        for type_content in self.content.iter() {
            if !type_content.is_ready() {
                return false;
            }
        }
        true
    }
}

impl IsReady for ArrayU8ToFill {
    fn is_ready(&self) -> bool {
        self.content.len() as u32 == self.len
    }
}

impl IsReady for ArrayRegularToFill {
    fn is_ready(&self) -> bool {
        if self.content.len() as u32 != self.len {
            return false;
        }
        for type_content in self.content.iter() {
            if !type_content.is_ready() {
                return false;
            }
        }
        true
    }
}

impl IsReady for SpecialTypeToFill {
    fn is_ready(&self) -> bool {
        match &self {
            SpecialTypeToFill::AccountId32(a) => a.is_some(),
            SpecialTypeToFill::Era(era_to_fill) => match era_to_fill {
                EraToFill::Immortal => true,
                EraToFill::Mortal { period, phase } => period.is_some() & phase.is_some(),
            },
            SpecialTypeToFill::PerU16 {
                value,
                is_compact: _,
            } => value.is_some(),
            SpecialTypeToFill::Perbill {
                value,
                is_compact: _,
            } => value.is_some(),
            SpecialTypeToFill::Percent {
                value,
                is_compact: _,
            } => value.is_some(),
            SpecialTypeToFill::Permill {
                value,
                is_compact: _,
            } => value.is_some(),
            SpecialTypeToFill::Perquintill {
                value,
                is_compact: _,
            } => value.is_some(),
            SpecialTypeToFill::PublicEd25519(a) => a.is_some(),
            SpecialTypeToFill::PublicSr25519(a) => a.is_some(),
            SpecialTypeToFill::PublicEcdsa(a) => a.is_some(),
        }
    }
}
