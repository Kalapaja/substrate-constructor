use external_memory_tools::ExternalMemory;
use frame_metadata::{v14::RuntimeMetadataV14, v15::RuntimeMetadataV15};
use substrate_parser::{
    cards::ParsedData,
    decode_all_as_type,
    error::{MetaStructureErrorV14, MetaVersionErrorPallets},
    special_indicators::SpecialtyUnsignedInteger,
    traits::{
        version_constant_data_and_ty_v14, version_constant_data_and_ty_v15, AsCompleteMetadata,
    },
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Unsigned {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
}

pub trait AsFillMetadata<E: ExternalMemory>: AsCompleteMetadata<E> {
    fn defined_tx_version(&self) -> Option<Unsigned>;
    fn spec_version(&self) -> Result<Unsigned, Self::MetaStructureError>;
}

// TODO spec_version as unsigned may go eventually into substrate_parser.
// TODO version_constant_data_and_ty should become public in substrate_parser.

macro_rules! impl_as_fill_metadata {
    ($($ty: ty, $func: ident, $err: expr), *) => {
        $(
            impl <E: ExternalMemory> AsFillMetadata<E> for $ty {
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
    version_constant_data_and_ty_v14,
    MetaStructureErrorV14::Version(MetaVersionErrorPallets::RuntimeVersionNotDecodeable)
);
impl_as_fill_metadata!(
    RuntimeMetadataV15,
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
