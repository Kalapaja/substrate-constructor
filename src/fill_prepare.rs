use bitvec::prelude::{BitVec, Lsb0, Msb0};
use external_memory_tools::ExternalMemory;
use num_bigint::{BigInt, BigUint};
//use parity_scale_codec::{Compact, Encode};
use scale_info::{
    form::PortableForm, interner::UntrackedSymbol, Field, TypeDef, TypeDefBitSequence,
    TypeDefPrimitive, Variant,
};
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use substrate_parser::{
    additional_types::{AccountId32, Era, PublicEcdsa, PublicEd25519, PublicSr25519},
    cards::{Documented, Info},
    decoding_sci::{find_bit_order_ty, husk_type, FoundBitOrder, ResolvedTy, Ty},
    error::RegistryError,
    propagated::{Checker, Propagated, SpecialtySet},
    special_indicators::{SpecialtyTypeHinted, SpecialtyUnsignedInteger},
    traits::{AsMetadata, ResolveType},
};

use std::any::TypeId;

use crate::error::ErrorFixMe;

#[derive(Debug)]
pub struct VariantSelected {
    pub docs: String,
    pub fields_to_fill: Vec<FieldToFill>,
    pub index: u8,
    pub name: String,
}

#[derive(Debug)]
pub struct FieldToFill {
    pub type_to_fill: TypeToFill,
    pub field_docs: String,
    pub field_name: Option<String>,
    pub type_name: Option<String>,
}

#[derive(Debug)]
pub struct TypeToFill {
    pub content: TypeContentToFill,
    pub info: Vec<Info>,
}

#[derive(Debug)]
pub enum TypeContentToFill {
    Array(ArrayToFill),
    BitSequence(BitSequenceToFill),
    Composite(Vec<FieldToFill>),
    Primitive(PrimitiveToFill),
    Sequence(SequenceToFill),
    SpecialType(SpecialTypeToFill),
    Tuple(Vec<TypeToFill>),
    Variant(VariantSelector),
}

#[derive(Debug)]
pub enum VariantSelector {
    Selected(VariantSelected),
    NeedIndex(Vec<Variant<PortableForm>>),
}

#[derive(Debug)]
pub enum BitSequenceToFill {
    BitVecU8Lsb0(Option<BitVec<u8, Lsb0>>),
    BitVecU16Lsb0(Option<BitVec<u16, Lsb0>>),
    BitVecU32Lsb0(Option<BitVec<u32, Lsb0>>),
    #[cfg(target_pointer_width = "64")]
    BitVecU64Lsb0(Option<BitVec<u64, Lsb0>>),
    BitVecU8Msb0(Option<BitVec<u8, Msb0>>),
    BitVecU16Msb0(Option<BitVec<u16, Msb0>>),
    BitVecU32Msb0(Option<BitVec<u32, Msb0>>),
    #[cfg(target_pointer_width = "64")]
    BitVecU64Msb0(Option<BitVec<u64, Msb0>>),
}

#[derive(Debug)]
pub enum PrimitiveToFill {
    CompactUnsigned(SpecialtyUnsignedToFill),
    Regular(RegularPrimitiveToFill),
    Unsigned(SpecialtyUnsignedToFill),
}

#[derive(Debug)]
pub enum RegularPrimitiveToFill {
    Bool(Option<bool>),
    Char(Option<char>),
    I8(Option<i8>),
    I16(Option<i16>),
    I32(Option<i32>),
    I64(Option<i64>),
    I128(Option<i128>),
    I256(Option<BigInt>),
    Str(String),
    U256(Option<BigUint>),
}

#[derive(Debug)]
pub enum UnsignedToFill {
    U8(Option<u8>),
    U16(Option<u16>),
    U32(Option<u32>),
    U64(Option<u64>),
    U128(Option<u128>),
}

#[derive(Debug)]
pub struct SpecialtyUnsignedToFill {
    pub content: UnsignedToFill,
    pub specialty: SpecialtyUnsignedInteger,
}

#[derive(Debug)]
pub struct SequenceToFill {
    pub content: SetInProgress,
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
}

#[derive(Debug)]
pub struct ArrayToFill {
    pub sequence: SequenceToFill,
    pub len: u32,
}

#[derive(Debug)]
pub enum SetInProgress {
    U8(Vec<u8>),
    Regular(Vec<TypeContentToFill>),
}

#[derive(Debug)]
pub enum SpecialTypeToFill {
    AccountId32(Option<AccountId32>),
    Era(Option<Era>),
    PerU16 {
        value: Option<PerU16>,
        is_compact: bool,
    },
    Perbill {
        value: Option<Perbill>,
        is_compact: bool,
    },
    Percent {
        value: Option<Percent>,
        is_compact: bool,
    },
    Permill {
        value: Option<Permill>,
        is_compact: bool,
    },
    Perquintill {
        value: Option<Perquintill>,
        is_compact: bool,
    },
    PublicEd25519(Option<PublicEd25519>),
    PublicSr25519(Option<PublicSr25519>),
    PublicEcdsa(Option<PublicEcdsa>),
}

pub trait FillPrimitive {
    fn primitive_to_fill(specialty_set: &SpecialtySet) -> Result<PrimitiveToFill, RegistryError>;
}

macro_rules! impl_regular_fill_primitive {
    ($($ty: ty, $variant: ident), *) => {
        $(
            impl FillPrimitive for $ty {
                fn primitive_to_fill(specialty_set: &SpecialtySet) -> Result<PrimitiveToFill, RegistryError> {
                    if let Some(id) = specialty_set.compact_at {Err(RegistryError::UnexpectedCompactInsides{id})}
                    else {Ok(PrimitiveToFill::Regular(RegularPrimitiveToFill::$variant(None)))}
                }
            }
        )*
    }
}

impl_regular_fill_primitive!(bool, Bool);
impl_regular_fill_primitive!(char, Char);
impl_regular_fill_primitive!(i8, I8);
impl_regular_fill_primitive!(i16, I16);
impl_regular_fill_primitive!(i32, I32);
impl_regular_fill_primitive!(i64, I64);
impl_regular_fill_primitive!(i128, I128);
impl_regular_fill_primitive!(BigInt, I256);
impl_regular_fill_primitive!(BigUint, U256);

impl FillPrimitive for String {
    fn primitive_to_fill(specialty_set: &SpecialtySet) -> Result<PrimitiveToFill, RegistryError> {
        specialty_set.reject_compact()?;
        Ok(PrimitiveToFill::Regular(RegularPrimitiveToFill::Str(
            String::new(),
        )))
    }
}

macro_rules! impl_unsigned_fill_primitive {
    ($($ty: ty, $variant: ident), *) => {
        $(
            impl FillPrimitive for $ty {
                fn primitive_to_fill(specialty_set: &SpecialtySet) -> Result<PrimitiveToFill, RegistryError> {
                    let specialty = specialty_set.unsigned_integer();
                    let specialty_unsigned_to_fill = SpecialtyUnsignedToFill {
                        content: UnsignedToFill::$variant(None),
                        specialty,
                    };
                    if specialty_set.compact_at.is_some() {
                        Ok(PrimitiveToFill::CompactUnsigned(specialty_unsigned_to_fill))
                    }
                    else {
                        Ok(PrimitiveToFill::Unsigned(specialty_unsigned_to_fill))
                    }
                }
            }
        )*
    }
}

impl_unsigned_fill_primitive!(u8, U8);
impl_unsigned_fill_primitive!(u16, U16);
impl_unsigned_fill_primitive!(u32, U32);
impl_unsigned_fill_primitive!(u64, U64);
impl_unsigned_fill_primitive!(u128, U128);

pub trait FillSpecial {
    fn special_to_fill(specialty_set: &SpecialtySet) -> Result<SpecialTypeToFill, RegistryError>;
}

macro_rules! impl_fill_special {
    ($($ty: tt), *) => {
        $(
            impl FillSpecial for $ty {
                fn special_to_fill(specialty_set: &SpecialtySet) -> Result<SpecialTypeToFill, RegistryError> {
                    specialty_set.reject_compact()?;
                    Ok(SpecialTypeToFill::$ty(None))
                }
            }
        )*
    }
}

impl_fill_special!(Era, AccountId32, PublicEd25519, PublicSr25519, PublicEcdsa);

macro_rules! impl_fill_special_with_compact {
    ($($ty: tt), *) => {
        $(
            impl FillSpecial for $ty {
                fn special_to_fill(specialty_set: &SpecialtySet) -> Result<SpecialTypeToFill, RegistryError> {
                    Ok(SpecialTypeToFill::$ty{value: None, is_compact: specialty_set.compact_at.is_some()})
                }
            }
        )*
    }
}

impl_fill_special_with_compact!(PerU16, Perbill, Percent, Permill, Perquintill);

pub fn prepare_primitive(
    found_ty: &TypeDefPrimitive,
    specialty_set: &SpecialtySet,
) -> Result<PrimitiveToFill, RegistryError> {
    match found_ty {
        TypeDefPrimitive::Bool => bool::primitive_to_fill(specialty_set),
        TypeDefPrimitive::Char => char::primitive_to_fill(specialty_set),
        TypeDefPrimitive::Str => String::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I8 => i8::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I16 => i16::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I32 => i32::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I64 => i64::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I128 => i128::primitive_to_fill(specialty_set),
        TypeDefPrimitive::I256 => BigInt::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U8 => u8::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U16 => u16::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U32 => u32::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U64 => u64::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U128 => u128::primitive_to_fill(specialty_set),
        TypeDefPrimitive::U256 => BigUint::primitive_to_fill(specialty_set),
    }
}

#[derive(Debug)]
pub enum MultiAddress {
    Ed25519(PublicEd25519),
    Sr25519(PublicSr25519),
    Ecdsa(PublicEcdsa),
}

#[derive(Debug)]
pub struct TransactionToFill {
    pub author: Option<MultiAddress>,
    pub call: TypeToFill,
    pub extensions: Vec<TypeToFill>,
}

impl TransactionToFill {
    pub fn init<E, M>(ext_memory: &mut E, metadata: &M) -> Result<Self, ErrorFixMe<E, M>>
    where
        E: ExternalMemory,
        M: AsMetadata<E>,
    {
        let registry = metadata.types();
        let extrinsic_type_params = metadata.extrinsic_type_params().map_err(ErrorFixMe::MetaStructure)?;
        let signed_extensions = metadata.signed_extensions().map_err(ErrorFixMe::MetaStructure)?;

        let call = prepare_type::<E, M>(
            &Ty::Symbol(&extrinsic_type_params.call_ty),
            ext_memory,
            &registry,
            Propagated::new(),
        )?;

        let mut extensions = Vec::new();

        for signed_extensions_metadata in signed_extensions.iter() {
            extensions.push(prepare_type::<E, M>(
                &Ty::Symbol(&signed_extensions_metadata.ty),
                ext_memory,
                &registry,
                Propagated::from_ext_meta(signed_extensions_metadata),
            )?)
        }
        for signed_extensions_metadata in signed_extensions.iter() {
            extensions.push(prepare_type::<E, M>(
                &Ty::Symbol(&signed_extensions_metadata.additional_signed),
                ext_memory,
                &registry,
                Propagated::from_ext_meta(signed_extensions_metadata),
            )?)
        }
        Ok(TransactionToFill {
            author: None,
            call,
            extensions,
        })
    }
}

pub fn prepare_type<E, M>(
    ty_input: &Ty,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
    mut propagated: Propagated,
) -> Result<TypeToFill, RegistryError>
where
    E: ExternalMemory,
    M: AsMetadata<E>,
{
    let (ty, id) = match ty_input {
        Ty::Resolved(resolved_ty) => (resolved_ty.ty.to_owned(), resolved_ty.id),
        Ty::Symbol(ty_symbol) => (registry.resolve_ty(ty_symbol.id, ext_memory)?, ty_symbol.id),
    };
    let info_ty = Info::from_ty(&ty);
    propagated.add_info(&info_ty);

    match SpecialtyTypeHinted::from_type(&ty) {
        SpecialtyTypeHinted::AccountId32 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(AccountId32::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::Era => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(Era::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::PerU16 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(PerU16::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::Perbill => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(Perbill::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::Percent => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(Percent::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::Permill => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(Permill::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::Perquintill => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(Perquintill::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::PublicEd25519 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(PublicEd25519::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::PublicSr25519 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(PublicSr25519::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::PublicEcdsa => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(PublicEcdsa::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        _ => match &ty.type_def {
            TypeDef::Composite(x) => Ok(TypeToFill {
                content: TypeContentToFill::Composite(prepare_fields::<E, M>(
                    &x.fields,
                    ext_memory,
                    registry,
                    propagated.checker,
                )?),
                info: propagated.info,
            }),
            TypeDef::Variant(x) => Ok(TypeToFill {
                content: TypeContentToFill::Variant(VariantSelector::NeedIndex(
                    x.variants.to_vec(),
                )),
                info: propagated.info,
            }),
            TypeDef::Sequence(x) => {
                let mut info = Vec::new();
                info.append(&mut propagated.info);
                let sequence_to_fill =
                    prepare_elements_set::<E, M>(&x.type_param, ext_memory, registry, propagated)?;
                Ok(TypeToFill {
                    content: TypeContentToFill::Sequence(sequence_to_fill),
                    info,
                })
            }
            TypeDef::Array(x) => {
                let mut info = Vec::new();
                info.append(&mut propagated.info);
                let sequence_to_fill =
                    prepare_elements_set::<E, M>(&x.type_param, ext_memory, registry, propagated)?;
                Ok(TypeToFill {
                    content: TypeContentToFill::Array(ArrayToFill {
                        sequence: sequence_to_fill,
                        len: x.len,
                    }),
                    info,
                })
            }
            TypeDef::Tuple(x) => {
                if x.fields.len() > 1 {
                    propagated.reject_compact()?;
                    propagated.forget_hint();
                }
                let mut tuple_set = Vec::new();
                for inner_ty_symbol in x.fields.iter() {
                    let id = inner_ty_symbol.id;
                    let ty = registry.resolve_ty(id, ext_memory)?;
                    let tuple_element = prepare_type::<E, M>(
                        &Ty::Resolved(ResolvedTy {
                            ty: ty.to_owned(),
                            id,
                        }),
                        ext_memory,
                        registry,
                        Propagated::for_ty(&propagated.checker, &ty, id)?,
                    )?;
                    tuple_set.push(tuple_element);
                }
                Ok(TypeToFill {
                    content: TypeContentToFill::Tuple(tuple_set),
                    info: propagated.info,
                })
            }
            TypeDef::Primitive(x) => Ok(TypeToFill {
                content: TypeContentToFill::Primitive(prepare_primitive(
                    x,
                    &propagated.checker.specialty_set,
                )?),
                info: propagated.info,
            }),
            TypeDef::Compact(x) => {
                propagated.reject_compact()?;
                propagated.checker.specialty_set.compact_at = Some(id);
                propagated.checker.check_id(x.type_param.id)?;
                prepare_type::<E, M>(&Ty::Symbol(&x.type_param), ext_memory, registry, propagated)
            }
            TypeDef::BitSequence(x) => {
                propagated.reject_compact()?;
                let bit_sequence = prepare_bit_sequence::<E, M>(x, id, ext_memory, registry)?;
                Ok(TypeToFill {
                    content: TypeContentToFill::BitSequence(bit_sequence),
                    info: propagated.info,
                })
            }
        },
    }
}

pub fn prepare_fields<E, M>(
    fields: &[Field<PortableForm>],
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
    mut checker: Checker,
) -> Result<Vec<FieldToFill>, RegistryError>
where
    E: ExternalMemory,
    M: AsMetadata<E>,
{
    if fields.len() > 1 {
        // Only single-field structs can be processed as a compact.
        // Note: compact flag was already checked in enum processing at this
        // point.
        checker.reject_compact()?;

        // `Hint` remains relevant only if single-field struct is processed.
        // Note: checker gets renewed when fields of enum are processed.
        checker.forget_hint();
    }
    let mut out: Vec<FieldToFill> = Vec::new();
    for field in fields.iter() {
        let field_name = field.name.to_owned();
        let type_name = field.type_name.to_owned();
        let type_to_fill = prepare_type::<E, M>(
            &Ty::Symbol(&field.ty),
            ext_memory,
            registry,
            Propagated::for_field(&checker, field)?,
        )?;
        out.push(FieldToFill {
            type_to_fill,
            field_docs: field.collect_docs(),
            field_name,
            type_name,
        })
    }
    Ok(out)
}

pub fn prepare_elements_set<E, M>(
    element: &UntrackedSymbol<TypeId>,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
    propagated: Propagated,
) -> Result<SequenceToFill, RegistryError>
where
    E: ExternalMemory,
    M: AsMetadata<E>,
{
    propagated.reject_compact()?;

    let husked = husk_type::<E, M>(element, registry, ext_memory, propagated.checker)?;
    let resolved_ty = ResolvedTy {
        ty: husked.ty.to_owned(),
        id: husked.id,
    };
    let content = {
        if husked.ty.type_def == TypeDef::Primitive(TypeDefPrimitive::U8) {
            SetInProgress::U8(Vec::new())
        } else {
            SetInProgress::Regular(Vec::new())
        }
    };
    Ok(SequenceToFill {
        content,
        info_element: husked.info.to_owned(),
        resolved_ty,
        checker: husked.checker,
    })
}

pub fn prepare_bit_sequence<E, M>(
    bit_ty: &TypeDefBitSequence<PortableForm>,
    id: u32,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
) -> Result<BitSequenceToFill, RegistryError>
where
    E: ExternalMemory,
    M: AsMetadata<E>,
{
    // BitOrder
    let bitorder = find_bit_order_ty::<E, M>(bit_ty, id, ext_memory, registry)?;

    // BitStore
    let bitstore_type = registry.resolve_ty(bit_ty.bit_store_type.id, ext_memory)?;

    match bitstore_type.type_def {
        TypeDef::Primitive(TypeDefPrimitive::U8) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceToFill::BitVecU8Lsb0(None)),
            FoundBitOrder::Msb0 => Ok(BitSequenceToFill::BitVecU8Msb0(None)),
        },
        TypeDef::Primitive(TypeDefPrimitive::U16) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceToFill::BitVecU16Lsb0(None)),
            FoundBitOrder::Msb0 => Ok(BitSequenceToFill::BitVecU16Msb0(None)),
        },
        TypeDef::Primitive(TypeDefPrimitive::U32) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceToFill::BitVecU32Lsb0(None)),
            FoundBitOrder::Msb0 => Ok(BitSequenceToFill::BitVecU32Msb0(None)),
        },
        #[cfg(target_pointer_width = "64")]
        TypeDef::Primitive(TypeDefPrimitive::U64) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceToFill::BitVecU64Lsb0(None)),
            FoundBitOrder::Msb0 => Ok(BitSequenceToFill::BitVecU64Msb0(None)),
        },
        _ => Err(RegistryError::NotBitStoreType { id }),
    }
}

pub fn select_variant<E, M>(
    variants: &[Variant<PortableForm>],
    index: u8,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
) -> Result<VariantSelected, ErrorFixMe<E, M>>
where
    E: ExternalMemory,
    M: AsMetadata<E>,
{
    let mut found_variant = None;
    for variant in variants.iter() {
        if variant.index == index {
            found_variant = Some(variant);
            break;
        }
    }
    match found_variant {
        Some(variant) => {
            let name = variant.name.to_owned();
            let docs = variant.collect_docs();
            let fields_to_fill =
                prepare_fields::<E, M>(&variant.fields, ext_memory, registry, Checker::new())?;
            Ok(VariantSelected {
                docs,
                fields_to_fill,
                index,
                name,
            })
        }
        None => Err(ErrorFixMe::UnexpectedVariantIndex),
    }
}
