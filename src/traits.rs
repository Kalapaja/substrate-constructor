use external_memory_tools::ExternalMemory;
use frame_metadata::{
    v14::{
        PalletCallMetadata, PalletConstantMetadata, PalletErrorMetadata, PalletEventMetadata,
        PalletMetadata as PalletMetadataV14, PalletStorageMetadata, RuntimeMetadataV14,
    },
    v15::{PalletMetadata as PalletMetadataV15, RuntimeMetadataV15},
};
use scale_info::form::PortableForm;
use substrate_parser::{
    cards::ParsedData,
    decode_all_as_type,
    error::{MetaStructureErrorV14, MetaVersionErrorPallets},
    special_indicators::SpecialtyUnsignedInteger,
    traits::{
        version_constant_data_and_ty_v14, version_constant_data_and_ty_v15, AsCompleteMetadata,
    },
};

use std::fmt::Debug;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Unsigned {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

pub trait AsPalletMetadata<E: ExternalMemory> {
    fn name(&self) -> String;
    fn storage(&self) -> Option<PalletStorageMetadata<PortableForm>>;
    fn calls(&self) -> Option<PalletCallMetadata<PortableForm>>;
    fn event(&self) -> Option<PalletEventMetadata<PortableForm>>;
    fn constants(&self) -> Vec<PalletConstantMetadata<PortableForm>>;
    fn error(&self) -> Option<PalletErrorMetadata<PortableForm>>;
    fn index(&self) -> u8;
}

macro_rules! impl_as_pallet_metadata {
    ($($ty:ty), *) => {
        $(
            impl <E: ExternalMemory> AsPalletMetadata<E> for $ty {
                fn name(&self) -> String {
                    self.name.to_owned()
                }
                fn storage(&self) -> Option<PalletStorageMetadata<PortableForm>> {
                    self.storage.to_owned()
                }
                fn calls(&self) -> Option<PalletCallMetadata<PortableForm>> {
                    self.calls.to_owned()
                }
                fn event(&self) -> Option<PalletEventMetadata<PortableForm>> {
                    self.event.to_owned()
                }
                fn constants(&self) -> Vec<PalletConstantMetadata<PortableForm>> {
                    self.constants.to_owned()
                }
                fn error(&self) -> Option<PalletErrorMetadata<PortableForm>> {
                    self.error.to_owned()
                }
                fn index(&self) -> u8 {
                    self.index
                }
            }
        )*
    }
}

impl_as_pallet_metadata!(
    PalletMetadataV14<PortableForm>,
    PalletMetadataV15<PortableForm>
);

pub trait AsFillMetadata<E: ExternalMemory>: AsCompleteMetadata<E> {
    type PalletMetadata: AsPalletMetadata<E> + Clone + Debug;
    fn pallets(&self) -> Vec<Self::PalletMetadata>;
    fn defined_tx_version(&self) -> Option<Unsigned>;
    fn spec_version(&self) -> Result<Unsigned, Self::MetaStructureError>;
}

// TODO spec_version as unsigned may go eventually into substrate_parser.

macro_rules! impl_as_fill_metadata {
    ($($ty:ty, $ty_pallet_metadata:ty, $func:ident, $err:expr), *) => {
        $(
            impl <E: ExternalMemory> AsFillMetadata<E> for $ty {
                type PalletMetadata = $ty_pallet_metadata;
                fn pallets(&self) -> Vec<Self::PalletMetadata> {
                    self.pallets.to_owned()
                }
                fn defined_tx_version(&self) -> Option<Unsigned> {
                    match $func(&self.pallets) {
                        Ok((version_data, version_ty)) => {
                            match decode_all_as_type::<&[u8], (), $ty>(
                                &version_ty,
                                &version_data.as_ref(),
                                &mut (),
                                &self.types,
                            ) {
                                Ok(extended_data) => tx_version(
                                    extended_data.data,
                                ),
                                Err(_) => None,
                            }
                        },
                        Err(_) => None,
                    }
                }
                fn spec_version(&self) -> Result<Unsigned, Self::MetaStructureError> {
                    let (version_data, version_ty) = $func(&self.pallets)?;
                    match decode_all_as_type::<&[u8], (), $ty>(
                        &version_ty,
                        &version_data.as_ref(),
                        &mut (),
                        &self.types,
                    ) {
                        Ok(extended_data) => Ok(spec_version(
                            extended_data.data,
                        )?),
                        Err(_) => Err($err),
                    }
                }
            }
        )*
    }
}

impl_as_fill_metadata!(
    RuntimeMetadataV14,
    PalletMetadataV14<PortableForm>,
    version_constant_data_and_ty_v14,
    MetaStructureErrorV14::Version(MetaVersionErrorPallets::RuntimeVersionNotDecodeable)
);
impl_as_fill_metadata!(
    RuntimeMetadataV15,
    PalletMetadataV15<PortableForm>,
    version_constant_data_and_ty_v15,
    MetaVersionErrorPallets::RuntimeVersionNotDecodeable
);

/// Extract [`Unsigned`] `spec_version` from `Version` parsed data.
fn spec_version(parsed_data: ParsedData) -> Result<Unsigned, MetaVersionErrorPallets> {
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

/// Extract [`Unsigned`] `tx_version` from `Version` parsed data.
///
/// It is not an error to not have `tx_version`.
fn tx_version(parsed_data: ParsedData) -> Option<Unsigned> {
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
                        return None;
                    }
                }
                ParsedData::PrimitiveU16 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U16(*value))
                    } else {
                        return None;
                    }
                }
                ParsedData::PrimitiveU32 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U32(*value))
                    } else {
                        return None;
                    }
                }
                ParsedData::PrimitiveU64 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U64(*value))
                    } else {
                        return None;
                    }
                }
                ParsedData::PrimitiveU128 {
                    value,
                    specialty: SpecialtyUnsignedInteger::TxVersion,
                } => {
                    if tx_version.is_none() {
                        tx_version = Some(Unsigned::U128(*value))
                    } else {
                        return None;
                    }
                }
                _ => (),
            }
        }
    } else {
        return None;
    }
    tx_version
}
