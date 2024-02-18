use std::any::TypeId;

use external_memory_tools::ExternalMemory;
use frame_metadata::{
    v14::{
        PalletMetadata as PalletMetadataV14, RuntimeMetadataV14,
        SignedExtensionMetadata as SignedExtensionMetadataV14,
    },
    v15::{
        PalletMetadata as PalletMetadataV15, RuntimeMetadataV15,
        SignedExtensionMetadata as SignedExtensionMetadataV15,
    },
};
use scale_info::{
    form::PortableForm, interner::UntrackedSymbol, PortableRegistry, Type, TypeDef,
    TypeDefPrimitive, TypeParameter,
};
use substrate_parser::{
    cards::ParsedData,
    error::MetaVersionErrorPallets,
    special_indicators::SpecialtyUnsignedInteger,
    traits::{AsCompleteMetadata, ResolveType, SpecNameVersion},
};

pub enum Unsigned {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

pub trait AsFillMetadata<E: ExternalMemory>: AsCompleteMetadata<E> {
    fn tx_version(&self) -> Option<Unsigned>;
//    fn spec_version(&self) -> Result<Unsigned, Self::MetaStructureError>;
}

/// Find `Version` constant and its type in `System` pallet.
macro_rules! impl_runtime_version_data_and_ty {
    ($(#[$attr:meta] $ty: ty, $func: ident), *) => {
        $(
            #[$attr]
            fn $func(pallets: &[$ty]) -> Result<(Vec<u8>, UntrackedSymbol<TypeId>), MetaVersionErrorPallets> {
                let mut runtime_version_data_and_ty = None;
                let mut system_block = false;
                for pallet in pallets.iter() {
                    if pallet.name == "System" {
                        system_block = true;
                        for constant in pallet.constants.iter() {
                            if constant.name == "Version" {
                                runtime_version_data_and_ty = Some((constant.value.to_vec(), constant.ty))
                            }
                        }
                        break;
                    }
                }
                if !system_block {
                    return Err(MetaVersionErrorPallets::NoSystemPallet);
                }
                runtime_version_data_and_ty.ok_or(MetaVersionErrorPallets::NoVersionInConstants)
            }
        )*
    }
}

impl_runtime_version_data_and_ty!(
    /// Find `Version` constant and its type in `System` pallet for `V14` metadata.
    PalletMetadataV14<PortableForm>,
    runtime_version_data_and_ty_v14
);
impl_runtime_version_data_and_ty!(
    /// Find `Version` constant and its type in `System` pallet for `V15` metadata.
    PalletMetadataV15<PortableForm>,
    runtime_version_data_and_ty_v15
);

/// Extract [`Unsigned`] `spec_version` from `Version` parsed data.
fn spec_version(
    parsed_data: ParsedData,
) -> Result<Unsigned, MetaVersionErrorPallets> {
    let mut spec_version = None;

    if let ParsedData::Composite(fields) = parsed_data {
        for field in fields.iter() {
            match &field.data.data {
                ParsedData::PrimitiveU8 {
                    value,
                    specialty: SpecialtyUnsignedInteger::SpecVersion,
                } => {
                    if spec_version.is_none() {
                        spec_version = Some(Unsigned::U8(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU16 {
                    value,
                    specialty: SpecialtyUnsignedInteger::SpecVersion,
                } => {
                    if spec_version.is_none() {
                        spec_version = Some(Unsigned::U16(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU32 {
                    value,
                    specialty: SpecialtyUnsignedInteger::SpecVersion,
                } => {
                    if spec_version.is_none() {
                        spec_version = Some(Unsigned::U32(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU64 {
                    value,
                    specialty: SpecialtyUnsignedInteger::SpecVersion,
                } => {
                    if spec_version.is_none() {
                        spec_version = Some(Unsigned::U64(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU128 {
                    value,
                    specialty: SpecialtyUnsignedInteger::SpecVersion,
                } => {
                    if spec_version.is_none() {
                        spec_version = Some(Unsigned::U128(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                _ => (),
            }
        }
    } else {
        return Err(MetaVersionErrorPallets::UnexpectedRuntimeVersionFormat);
    }
    spec_version.ok_or(MetaVersionErrorPallets::NoSpecVersionIdentifier)
}

/// Extract [`Unsigned`] `spec_version` from `Version` parsed data.
fn tx_version(
    parsed_data: ParsedData,
) -> Result<Unsigned, MetaVersionErrorPallets> {
    let mut tx_version = None;

    if let ParsedData::Composite(fields) = parsed_data {
        for field in fields.iter() {
            match &field.data.data {
                ParsedData::PrimitiveU8 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U8(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU16 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U16(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU32 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U32(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU64 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U64(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                ParsedData::PrimitiveU128 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U128(*value))
                    } else {
                        return Err(MetaVersionErrorPallets::SpecVersionIdentifierTwice);
                    }
                }
                _ => (),
            }
        }
    } else {
        return Err(MetaVersionErrorPallets::UnexpectedRuntimeVersionFormat);
    }
    tx_version.ok_or(MetaVersionErrorPallets::NoSpecVersionIdentifier)
}
