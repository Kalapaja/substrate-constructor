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
    additional_types::{AccountId32, PublicEcdsa, PublicEd25519, PublicSr25519},
    cards::{Documented, Info},
    decoding_sci::{find_bit_order_ty, husk_type, FoundBitOrder, ResolvedTy, Ty},
    error::RegistryError,
    propagated::{Checker, Propagated, SpecialtySet},
    special_indicators::{SpecialtyTypeHinted, SpecialtyUnsignedInteger},
    traits::{AsCompleteMetadata, ResolveType},
};

use std::any::TypeId;

use crate::error::ErrorFixMe;

#[derive(Debug)]
pub struct VariantSelected {
    pub selector_index: usize,
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
    ArrayU8(ArrayU8ToFill),
    ArrayRegular(ArrayRegularToFill),
    BitSequence(BitSequenceToFill),
    Composite(Vec<FieldToFill>),
    Primitive(PrimitiveToFill),
    SequenceRegular(SequenceRegularToFill),
    SequenceU8(SequenceU8ToFill),
    SpecialType(SpecialTypeToFill),
    Tuple(Vec<TypeToFill>),
    Variant(VariantSelector),
    VariantEmpty,
}

#[derive(Debug)]
pub struct VariantSelector {
    pub available_variants: Vec<Variant<PortableForm>>,
    pub selected: VariantSelected,
}

impl VariantSelector {
    pub fn init<E: ExternalMemory, M: AsCompleteMetadata<E>>(
        variants: &[Variant<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<Self, RegistryError> {
        Self::new_at::<E, M>(variants, ext_memory, registry, 0)
    }
    pub fn new_at<E: ExternalMemory, M: AsCompleteMetadata<E>>(
        variants: &[Variant<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
        selector_index: usize,
    ) -> Result<Self, RegistryError> {
        // this panics if selector is out of bounds; fix this later;
        let variant = &variants[selector_index];
        let name = variant.name.to_owned();
        let docs = variant.collect_docs();
        let fields_to_fill =
            prepare_fields::<E, M>(&variant.fields, ext_memory, registry, Checker::new())?;
        let selected = VariantSelected {
            selector_index,
            docs,
            fields_to_fill,
            index: variant.index,
            name,
        };
        Ok(Self {
            available_variants: variants.to_owned(),
            selected,
        })
    }
    pub fn selector_up<E: ExternalMemory, M: AsCompleteMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<(), RegistryError> {
        let new_selector_index = {
            if self.selected.selector_index + 1 == self.available_variants.len() {
                0
            } else {
                self.selected.selector_index + 1
            }
        };
        *self = VariantSelector::new_at::<E, M>(
            &self.available_variants,
            ext_memory,
            registry,
            new_selector_index,
        )?;
        Ok(())
    }
    pub fn selector_down<E: ExternalMemory, M: AsCompleteMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<(), RegistryError> {
        let new_selector_index = {
            if self.selected.selector_index == 0 {
                self.available_variants.len() - 1
            } else {
                self.selected.selector_index - 1
            }
        };
        *self = VariantSelector::new_at::<E, M>(
            &self.available_variants,
            ext_memory,
            registry,
            new_selector_index,
        )?;
        Ok(())
    }
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
pub enum SequenceDraft {
    U8(SequenceDraftContent),
    Regular(SequenceDraftContent),
}

#[derive(Debug)]
pub struct SequenceDraftContent {
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
}

#[derive(Debug)]
pub struct SequenceU8ToFill {
    pub content: Vec<u8>,
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
}

#[derive(Debug)]
pub struct SequenceRegularToFill {
    pub content: Vec<TypeContentToFill>,
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
}

#[derive(Debug)]
pub struct ArrayU8ToFill {
    pub content: Vec<u8>,
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
    pub len: u32,
}

#[derive(Debug)]
pub struct ArrayRegularToFill {
    pub content: Vec<TypeContentToFill>,
    pub info_element: Vec<Info>,
    pub resolved_ty: ResolvedTy,
    pub checker: Checker,
    pub len: u32,
}

#[derive(Debug)]
pub enum SpecialTypeToFill {
    AccountId32(Option<AccountId32>),
    Era(EraToFill),
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

#[derive(Debug)]
pub enum EraToFill {
    Immortal,
    Mortal {
        period: Option<u64>,
        phase: Option<u64>,
    },
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

impl_fill_special!(AccountId32, PublicEd25519, PublicSr25519, PublicEcdsa);

impl FillSpecial for EraToFill {
    fn special_to_fill(specialty_set: &SpecialtySet) -> Result<SpecialTypeToFill, RegistryError> {
        specialty_set.reject_compact()?;
        Ok(SpecialTypeToFill::Era(EraToFill::Immortal))
    }
}

impl EraToFill {
    pub fn selector(&mut self) {
        match &self {
            EraToFill::Immortal => {
                *self = EraToFill::Mortal {
                    period: None,
                    phase: None,
                }
            }
            EraToFill::Mortal {
                period: _,
                phase: _,
            } => *self = EraToFill::Immortal,
        }
    }
}

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
pub struct TransactionToFill {
    pub author: TypeToFill,
    pub call: TypeToFill,
    pub extensions: Vec<TypeToFill>,
    pub signature: TypeToFill,
    pub extra: TypeToFill,
}

impl TransactionToFill {
    pub fn init<E, M>(ext_memory: &mut E, metadata: &M) -> Result<Self, ErrorFixMe<E, M>>
    where
        E: ExternalMemory,
        M: AsCompleteMetadata<E>,
    {
        let registry = metadata.types();
        let extrinsic_type_params = metadata
            .extrinsic_type_params()
            .map_err(ErrorFixMe::MetaStructure)?;
        let signed_extensions = metadata
            .signed_extensions()
            .map_err(ErrorFixMe::MetaStructure)?;

        let author = prepare_type::<E, M>(
            &Ty::Symbol(&extrinsic_type_params.address_ty),
            ext_memory,
            &registry,
            Propagated::new(),
        )?;

        let call = prepare_type::<E, M>(
            &Ty::Symbol(&extrinsic_type_params.call_ty),
            ext_memory,
            &registry,
            Propagated::new(),
        )?;

        let signature = prepare_type::<E, M>(
            &Ty::Symbol(&extrinsic_type_params.signature_ty),
            ext_memory,
            &registry,
            Propagated::new(),
        )?;

        let extra = prepare_type::<E, M>(
            &Ty::Symbol(&extrinsic_type_params.extra_ty),
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
            author,
            call,
            extensions,
            signature,
            extra,
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
    M: AsCompleteMetadata<E>,
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
            content: TypeContentToFill::SpecialType(EraToFill::special_to_fill(
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
            TypeDef::Variant(x) => {
                if x.variants.is_empty() {
                    Ok(TypeToFill {
                        content: TypeContentToFill::VariantEmpty,
                        info: propagated.info,
                    })
                } else {
                    Ok(TypeToFill {
                        content: TypeContentToFill::Variant(VariantSelector::init::<E, M>(
                            &x.variants,
                            ext_memory,
                            registry,
                        )?),
                        info: propagated.info,
                    })
                }
            }
            TypeDef::Sequence(x) => {
                let mut info = Vec::new();
                info.append(&mut propagated.info);
                let sequence_draft =
                    prepare_elements_set::<E, M>(&x.type_param, ext_memory, registry, propagated)?;
                match sequence_draft {
                    SequenceDraft::U8(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::SequenceU8(SequenceU8ToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            resolved_ty: sequence_draft_content.resolved_ty,
                            checker: sequence_draft_content.checker,
                        }),
                        info,
                    }),
                    SequenceDraft::Regular(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::SequenceRegular(SequenceRegularToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            resolved_ty: sequence_draft_content.resolved_ty,
                            checker: sequence_draft_content.checker,
                        }),
                        info,
                    }),
                }
            }
            TypeDef::Array(x) => {
                let mut info = Vec::new();
                info.append(&mut propagated.info);
                let sequence_draft =
                    prepare_elements_set::<E, M>(&x.type_param, ext_memory, registry, propagated)?;
                match sequence_draft {
                    SequenceDraft::U8(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::ArrayU8(ArrayU8ToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            resolved_ty: sequence_draft_content.resolved_ty,
                            checker: sequence_draft_content.checker,
                            len: x.len,
                        }),
                        info,
                    }),
                    SequenceDraft::Regular(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::ArrayRegular(ArrayRegularToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            resolved_ty: sequence_draft_content.resolved_ty,
                            checker: sequence_draft_content.checker,
                            len: x.len,
                        }),
                        info,
                    }),
                }
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
    M: AsCompleteMetadata<E>,
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
) -> Result<SequenceDraft, RegistryError>
where
    E: ExternalMemory,
    M: AsCompleteMetadata<E>,
{
    propagated.reject_compact()?;

    let husked = husk_type::<E, M>(element, registry, ext_memory, propagated.checker)?;
    let resolved_ty = ResolvedTy {
        ty: husked.ty.to_owned(),
        id: husked.id,
    };
    let sequence_draft_content = SequenceDraftContent {
        info_element: husked.info.to_owned(),
        resolved_ty,
        checker: husked.checker,
    };
    if husked.ty.type_def == TypeDef::Primitive(TypeDefPrimitive::U8) {
        Ok(SequenceDraft::U8(sequence_draft_content))
    } else {
        Ok(SequenceDraft::Regular(sequence_draft_content))
    }
}

pub fn prepare_bit_sequence<E, M>(
    bit_ty: &TypeDefBitSequence<PortableForm>,
    id: u32,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
) -> Result<BitSequenceToFill, RegistryError>
where
    E: ExternalMemory,
    M: AsCompleteMetadata<E>,
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
