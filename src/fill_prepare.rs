use bitvec::prelude::{BitVec, Lsb0, Msb0};
use external_memory_tools::ExternalMemory;
use num_bigint::{BigInt, BigUint};
use parity_scale_codec::Encode;
use primitive_types::H256;
use scale_info::{
    form::PortableForm, interner::UntrackedSymbol, Field, Type, TypeDef, TypeDefBitSequence,
    TypeDefPrimitive, Variant,
};
use sp_arithmetic::{PerU16, Perbill, Percent, Permill, Perquintill};
use substrate_parser::{
    additional_types::{
        AccountId32, PublicEcdsa, PublicEd25519, PublicSr25519, SignatureEcdsa, SignatureEd25519,
        SignatureSr25519,
    },
    cards::{Documented, Info},
    decoding_sci::{find_bit_order_ty, husk_type, FoundBitOrder, ResolvedTy, Ty},
    error::{ExtensionsError, RegistryError},
    propagated::{Checker, Propagated, SpecialtySet},
    special_indicators::{SpecialtyH256, SpecialtyTypeHinted, SpecialtyUnsignedInteger},
    traits::ResolveType,
};

use std::any::TypeId;

use crate::{
    error::ErrorFixMe,
    finalize::{Finalize, TypeContent},
    traits::{AsFillMetadata, Unsigned},
    try_fill::TryFill,
};

#[derive(Clone, Debug)]
pub struct VariantSelected {
    pub selector_index: usize,
    pub docs: String,
    pub fields_to_fill: Vec<FieldToFill>,
    pub index: u8,
    pub name: String,
}

#[derive(Clone, Debug)]
pub struct FieldToFill {
    pub type_to_fill: TypeToFill,
    pub field_docs: String,
    pub field_name: Option<String>,
    pub type_name: Option<String>,
}

#[derive(Clone, Debug)]
pub struct TypeToFill {
    pub content: TypeContentToFill,
    pub info: Vec<Info>,
}

#[derive(Clone, Debug)]
pub enum TypeContentToFill {
    ArrayU8(ArrayU8ToFill),
    ArrayRegular(ArrayRegularToFill),
    BitSequence(BitSequenceContent),
    Composite(Vec<FieldToFill>),
    Primitive(PrimitiveToFill),
    SequenceRegular(SequenceRegularToFill),
    SequenceU8(SequenceU8ToFill),
    SpecialType(SpecialTypeToFill),
    Tuple(Vec<TypeToFill>),
    Variant(VariantSelector),
    VariantEmpty,
}

#[derive(Clone, Debug)]
pub struct VariantSelector {
    pub available_variants: Vec<Variant<PortableForm>>,
    pub selected: VariantSelected,
}

impl VariantSelector {
    pub fn init<E: ExternalMemory, M: AsFillMetadata<E>>(
        variants: &[Variant<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<Self, RegistryError> {
        Self::new_at::<E, M>(variants, ext_memory, registry, 0)
    }
    pub fn new_at<E: ExternalMemory, M: AsFillMetadata<E>>(
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
    pub fn selector_up<E: ExternalMemory, M: AsFillMetadata<E>>(
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
    pub fn selector_down<E: ExternalMemory, M: AsFillMetadata<E>>(
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

#[derive(Clone, Debug)]
pub enum BitSequenceContent {
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
}

#[derive(Clone, Debug)]
pub enum PrimitiveToFill {
    CompactUnsigned(SpecialtyUnsignedToFill),
    Regular(RegularPrimitiveToFill),
    Unsigned(SpecialtyUnsignedToFill),
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub enum UnsignedToFill {
    U8(Option<u8>),
    U16(Option<u16>),
    U32(Option<u32>),
    U64(Option<u64>),
    U128(Option<u128>),
}

impl UnsignedToFill {
    pub fn into_unsigned(&self) -> Option<Unsigned> {
        match &self {
            UnsignedToFill::U8(a) => a.map(Unsigned::U8),
            UnsignedToFill::U16(a) => a.map(Unsigned::U16),
            UnsignedToFill::U32(a) => a.map(Unsigned::U32),
            UnsignedToFill::U64(a) => a.map(Unsigned::U64),
            UnsignedToFill::U128(a) => a.map(Unsigned::U128),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct SequenceU8ToFill {
    pub content: Vec<u8>,
    pub info_element: Vec<Info>,
}

#[derive(Clone, Debug)]
pub struct SequenceRegularToFill {
    pub content: Vec<TypeContentToFill>,
    pub info_element: Vec<Info>,
    pub ty: Type<PortableForm>,
    pub id: u32,
}

impl SequenceRegularToFill {
    pub fn remove_last_element(&mut self) {
        self.content.pop();
    }
    pub fn add_new_element<E: ExternalMemory, M: AsFillMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<(), RegistryError> {
        let element = prepare_type::<E, M>(
            &Ty::Resolved(ResolvedTy {
                ty: self.ty.to_owned(),
                id: self.id,
            }),
            ext_memory,
            registry,
            Propagated::new(),
        )?;
        self.content.push(element.content);
        Ok(())
    }
    pub fn set_number_of_elements<E: ExternalMemory, M: AsFillMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
        number_of_elements: usize,
    ) -> Result<(), RegistryError> {
        if self.content.len() <= number_of_elements {
            for _i in 0..number_of_elements - self.content.len() {
                self.add_new_element::<E, M>(ext_memory, registry)?;
            }
        } else {
            self.content.truncate(number_of_elements);
        }
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct ArrayU8ToFill {
    pub content: Vec<u8>,
    pub info_element: Vec<Info>,
    pub len: u32,
}

#[derive(Clone, Debug)]
pub struct ArrayRegularToFill {
    pub content: Vec<TypeContentToFill>,
    pub info_element: Vec<Info>,
    pub ty: Type<PortableForm>,
    pub id: u32,
    pub len: u32,
}

#[derive(Clone, Debug)]
pub enum SpecialTypeToFill {
    AccountId32(Option<AccountId32>),
    Era(EraToFill),
    H256(H256ToFill),
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
    SignatureEd25519(Option<SignatureEd25519>),
    SignatureSr25519(Option<SignatureSr25519>),
    SignatureEcdsa(Option<SignatureEcdsa>),
}

#[derive(Clone, Debug)]
pub struct H256ToFill {
    pub hash: Option<H256>,
    pub specialty: SpecialtyH256,
}

#[derive(Clone, Debug)]
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

impl_fill_special!(
    AccountId32,
    PublicEd25519,
    PublicSr25519,
    PublicEcdsa,
    SignatureEd25519,
    SignatureSr25519,
    SignatureEcdsa
);

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

impl FillSpecial for H256ToFill {
    fn special_to_fill(specialty_set: &SpecialtySet) -> Result<SpecialTypeToFill, RegistryError> {
        specialty_set.reject_compact()?;
        let specialty = specialty_set.hint.hash256();
        Ok(SpecialTypeToFill::H256(H256ToFill {
            hash: None,
            specialty,
        }))
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

#[derive(Clone, Debug)]
pub struct TransactionToFill {
    pub author: TypeToFill,
    pub call: TypeToFill,
    pub extensions: Vec<TypeToFill>,
    pub signature: TypeToFill,
    pub extra: Vec<usize>,
    pub genesis_hash: H256,
}

impl TransactionToFill {
    pub fn init<E, M>(
        ext_memory: &mut E,
        metadata: &M,
        genesis_hash: H256,
    ) -> Result<Self, ErrorFixMe<E, M>>
    where
        E: ExternalMemory,
        M: AsFillMetadata<E>,
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

        let mut extensions = Vec::new();
        let mut extensions_ty_ids = Vec::new();

        for signed_extensions_metadata in signed_extensions.iter() {
            extensions_ty_ids.push(signed_extensions_metadata.ty.id);
            extensions.push(prepare_type::<E, M>(
                &Ty::Symbol(&signed_extensions_metadata.ty),
                ext_memory,
                &registry,
                Propagated::from_ext_meta(signed_extensions_metadata),
            )?)
        }
        for signed_extensions_metadata in signed_extensions.iter() {
            extensions_ty_ids.push(signed_extensions_metadata.additional_signed.id);
            extensions.push(prepare_type::<E, M>(
                &Ty::Symbol(&signed_extensions_metadata.additional_signed),
                ext_memory,
                &registry,
                Propagated::from_ext_meta(signed_extensions_metadata),
            )?)
        }

        check_extensions(&extensions).map_err(ErrorFixMe::ExtensionsList)?;

        let extra = extra_indices_in_extensions::<E, M>(
            ext_memory,
            &registry,
            &extrinsic_type_params.extra_ty,
            &extensions_ty_ids,
        )?;

        let mut out = TransactionToFill {
            author,
            call,
            extensions,
            signature,
            extra,
            genesis_hash,
        };
        out.populate_genesis_hash(genesis_hash);
        out.populate_spec_version(&metadata.spec_version().map_err(ErrorFixMe::MetaStructure)?);
        if let Some(tx_version) = &metadata.defined_tx_version() {
            out.populate_tx_version(tx_version)
        }
        out.try_default_tip();
        out.try_default_signature_to_sr25519(ext_memory, metadata)
            .map_err(ErrorFixMe::Registry)?;
        Ok(out)
    }

    pub fn populate_block_hash(&mut self, block_hash: H256) {
        let hash = if era_is_immortal(&self.extensions) {
            self.genesis_hash
        } else {
            block_hash
        };

        self.populate_block_hash_helper(hash)
    }

    pub fn try_default_tip(&mut self) {
        for extension in self.extensions.iter_mut() {
            try_default_tip(&mut extension.content);
            if let TypeContentToFill::Composite(ref mut fields_to_fill) = extension.content {
                if fields_to_fill.len() == 1 {
                    try_default_tip(&mut fields_to_fill[0].type_to_fill.content);
                }
            }
        }
    }

    /// There could be different types of signature available for the chain.
    ///
    /// In case there is a set of variants to select from, check if sr25519
    /// variant is available and select it.
    pub fn try_default_signature_to_sr25519<E, M>(
        &mut self,
        ext_memory: &mut E,
        metadata: &M,
    ) -> Result<(), RegistryError>
    where
        E: ExternalMemory,
        M: AsFillMetadata<E>,
    {
        if let TypeContentToFill::Variant(ref mut variant_selector) = self.signature.content {
            let mut found_index = None;
            let registry = metadata.types();
            for (i, variant) in variant_selector.available_variants.iter().enumerate() {
                if variant.fields.len() == 1 {
                    let ty = registry.resolve_ty(variant.fields[0].ty.id, ext_memory)?;
                    if let SpecialtyTypeHinted::SignatureSr25519 =
                        SpecialtyTypeHinted::from_type(&ty)
                    {
                        found_index = Some(i);
                        break;
                    }
                }
            }
            if let Some(new_selector_index) = found_index {
                *variant_selector = VariantSelector::new_at::<E, M>(
                    &variant_selector.available_variants,
                    ext_memory,
                    &registry,
                    new_selector_index,
                )?;
            }
        }
        Ok(())
    }

    /// Either the type itself is sr25519 signature, the type is single field
    /// struct with sr25519 signature, or the variant selector with sr25519
    /// signature selected
    pub fn signature_is_sr25519(&self) -> bool {
        match self.signature.content {
            TypeContentToFill::Composite(ref fields_to_fill) => {
                if fields_to_fill.len() == 1 {
                    matches!(
                        fields_to_fill[0].type_to_fill.content,
                        TypeContentToFill::SpecialType(SpecialTypeToFill::SignatureSr25519(_))
                    )
                } else {
                    false
                }
            }
            TypeContentToFill::SpecialType(SpecialTypeToFill::SignatureSr25519(_)) => true,
            TypeContentToFill::Variant(ref variant_selector) => {
                if variant_selector.selected.fields_to_fill.len() == 1 {
                    matches!(
                        variant_selector.selected.fields_to_fill[0]
                            .type_to_fill
                            .content,
                        TypeContentToFill::SpecialType(SpecialTypeToFill::SignatureSr25519(_))
                    )
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Either the type itself is sr25519 compatible (AccountId32 or sr25519
    /// public key), the type is single field struct with one of those types,
    /// or the variant selector with one of those types selected
    pub fn author_as_sr25519_compatible(&self) -> Option<[u8; 32]> {
        match &self.author.content {
            TypeContentToFill::Composite(ref fields_to_fill) => {
                if fields_to_fill.len() == 1 {
                    match &fields_to_fill[0].type_to_fill.content {
                        TypeContentToFill::SpecialType(SpecialTypeToFill::AccountId32(a)) => {
                            a.clone().map(|b| b.0)
                        }
                        TypeContentToFill::SpecialType(SpecialTypeToFill::PublicSr25519(a)) => {
                            a.clone().map(|b| b.0)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            TypeContentToFill::SpecialType(SpecialTypeToFill::AccountId32(a)) => {
                a.clone().map(|b| b.0)
            }
            TypeContentToFill::SpecialType(SpecialTypeToFill::PublicSr25519(a)) => {
                a.clone().map(|b| b.0)
            }
            TypeContentToFill::Variant(ref variant_selector) => {
                if variant_selector.selected.fields_to_fill.len() == 1 {
                    match &variant_selector.selected.fields_to_fill[0]
                        .type_to_fill
                        .content
                    {
                        TypeContentToFill::SpecialType(SpecialTypeToFill::AccountId32(a)) => {
                            a.clone().map(|b| b.0)
                        }
                        TypeContentToFill::SpecialType(SpecialTypeToFill::PublicSr25519(a)) => {
                            a.clone().map(|b| b.0)
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    pub fn extrinsic_to_sign(&self) -> Option<ExtrinsicToSign> {
        if let Some(call) = self.call.finalize() {
            let mut extensions = Vec::new();
            for ext in self.extensions.iter() {
                if let Some(a) = ext.finalize() {
                    extensions.push(a);
                } else {
                    return None;
                }
            }
            Some(ExtrinsicToSign {
                call: call.to_owned(),
                extensions,
            })
        } else {
            None
        }
    }

    pub fn sign_this(&self) -> Option<Vec<u8>> {
        if let Some(extrinsic_to_sign) = self.extrinsic_to_sign() {
            let mut out = extrinsic_to_sign.call.encode();
            for ext in extrinsic_to_sign.extensions.iter() {
                out.extend_from_slice(&ext.encode())
            }
            Some(out)
        } else {
            None
        }
    }

    pub fn into_signer_transaction_format(&self) -> Option<Vec<u8>> {
        if let Some(extrinsic_to_sign) = self.extrinsic_to_sign() {
            let mut out = extrinsic_to_sign.call.encode().encode(); // call prefixed by call length
            for ext in extrinsic_to_sign.extensions.iter() {
                out.extend_from_slice(&ext.encode())
            }
            Some(out)
        } else {
            None
        }
    }

    pub fn output_qr_data_signer_style(&self) -> Option<Vec<u8>> {
        if let Some(author) = self.author_as_sr25519_compatible() {
            if let Some(signable) = self.into_signer_transaction_format() {
                let mut out = vec![0x53, 0x01, 0x00];
                out.extend_from_slice(&author);
                out.extend_from_slice(&signable);
                out.extend_from_slice(&self.genesis_hash.0);
                Some(out)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn signed_unchecked_extrinsic<E, M>(
        &self,
        metadata: &M,
    ) -> Result<Option<SignedUncheckedExtrinsic>, ErrorFixMe<E, M>>
    where
        E: ExternalMemory,
        M: AsFillMetadata<E>,
    {
        if let Some(author) = self.author.finalize() {
            if let Some(signature) = self.signature.finalize() {
                if let Some(call) = self.call.finalize() {
                    let mut extra = Vec::new();
                    for extra_index in self.extra.iter() {
                        let addition = self.extensions[*extra_index]
                            .finalize()
                            .ok_or(ErrorFixMe::UnfinalizedExtension)?;
                        extra.push(addition);
                    }
                    let extrinsic_version = metadata
                        .extrinsic_version()
                        .map_err(ErrorFixMe::MetaStructure)?;
                    return Ok(Some(SignedUncheckedExtrinsic {
                        version_byte: extrinsic_version | 0b1000_0000,
                        author: author.to_owned(),
                        signature: signature.to_owned(),
                        extra,
                        call: call.to_owned(),
                    }));
                }
            }
        }
        Ok(None)
    }
}

pub fn extra_indices_in_extensions<E: ExternalMemory, M: AsFillMetadata<E>>(
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
    extra_symbol: &UntrackedSymbol<TypeId>,
    extensions_ty_ids: &[u32],
) -> Result<Vec<usize>, ErrorFixMe<E, M>> {
    let extra_ty = registry
        .resolve_ty(extra_symbol.id, ext_memory)
        .map_err(ErrorFixMe::Registry)?;
    let extra_ty_ids: Vec<u32> = match &extra_ty.type_def {
        TypeDef::Composite(composite) => composite.fields.iter().map(|field| field.ty.id).collect(),
        TypeDef::Tuple(tuple) => tuple.fields.iter().map(|ty| ty.id).collect(),
        _ => return Err(ErrorFixMe::WrongExtraStructure),
    };
    let mut out = Vec::new();
    for extra_ty_id in extra_ty_ids.iter() {
        let new = extensions_ty_ids
            .iter()
            .position(|extension_ty_id| extension_ty_id == extra_ty_id)
            .ok_or(ErrorFixMe::ExtraNotInExtensions)?;
        out.push(new);
    }
    Ok(out)
}

#[derive(Clone, Debug)]
pub struct ExtrinsicToSign {
    pub call: TypeContent,
    pub extensions: Vec<TypeContent>,
}

#[derive(Clone, Debug)]
pub struct SignedUncheckedExtrinsic {
    pub version_byte: u8,
    pub author: TypeContent,
    pub signature: TypeContent,
    pub extra: Vec<TypeContent>,
    pub call: TypeContent,
}

macro_rules! populate {
    ($($func: ident, $filler: ty, $helper_func: ident), *) => {
        $(
            impl TransactionToFill {
                pub fn $func(&mut self, filler: $filler) {
                    for extension in self.extensions.iter_mut() {
                        if $helper_func(&mut extension.content, filler) {
                            break;
                        } else if let TypeContentToFill::Composite(ref mut fields_to_fill) = extension.content {
                            if fields_to_fill.len() == 1
                                && $helper_func(
                                    &mut fields_to_fill[0].type_to_fill.content,
                                    filler,
                                )
                            {
                                break;
                            }
                        }
                    }
                }
            }
        )*
    }
}

populate!(populate_genesis_hash, H256, genesis_hash_got_filled);
populate!(populate_block_hash_helper, H256, block_hash_got_filled);
populate!(populate_spec_version, &Unsigned, spec_version_got_filled);
populate!(populate_tx_version, &Unsigned, tx_version_got_filled);
populate!(populate_nonce, &Unsigned, nonce_got_filled);

macro_rules! got_filled_unsigned {
    ($($func: ident, $unsigned: ident), *) => {
        $(
            fn $func(content: &mut TypeContentToFill, unsigned: &Unsigned) -> bool {
                match content {
                    TypeContentToFill::Primitive(PrimitiveToFill::CompactUnsigned(
                        ref mut specialty_unsigned_to_fill,
                    )) => {
                        if let SpecialtyUnsignedInteger::$unsigned = specialty_unsigned_to_fill.specialty {
                            specialty_unsigned_to_fill
                                .content
                                .upd_from_unsigned(unsigned);
                            true
                        } else {
                            false
                        }
                    }
                    TypeContentToFill::Primitive(PrimitiveToFill::Unsigned(
                        ref mut specialty_unsigned_to_fill,
                    )) => {
                        if let SpecialtyUnsignedInteger::$unsigned = specialty_unsigned_to_fill.specialty {
                            specialty_unsigned_to_fill
                                .content
                                .upd_from_unsigned(unsigned);
                            true
                        } else {
                            false
                        }
                    }
                    _ => false,
                }
            }
        )*
    }
}

got_filled_unsigned!(spec_version_got_filled, SpecVersion);
got_filled_unsigned!(tx_version_got_filled, TxVersion);
got_filled_unsigned!(nonce_got_filled, Nonce);

fn try_default_tip(content: &mut TypeContentToFill) {
    match content {
        TypeContentToFill::Primitive(PrimitiveToFill::CompactUnsigned(
            ref mut specialty_unsigned_to_fill,
        )) => {
            if let SpecialtyUnsignedInteger::Tip = specialty_unsigned_to_fill.specialty {
                specialty_unsigned_to_fill.content.upd_from_str("0");
            }
        }
        TypeContentToFill::Primitive(PrimitiveToFill::Unsigned(
            ref mut specialty_unsigned_to_fill,
        )) => {
            if let SpecialtyUnsignedInteger::Tip = specialty_unsigned_to_fill.specialty {
                specialty_unsigned_to_fill.content.upd_from_str("0");
            }
        }
        _ => {}
    }
}

fn era_is_immortal(extensions: &[TypeToFill]) -> bool {
    let mut era_is_immortal = true;
    for extension in extensions.iter() {
        if let TypeContentToFill::SpecialType(SpecialTypeToFill::Era(era_to_fill)) =
            &extension.content
        {
            if let EraToFill::Mortal { .. } = era_to_fill {
                era_is_immortal = false;
            }
            break;
        }
        if let TypeContentToFill::Composite(fields_to_fill) = &extension.content {
            if fields_to_fill.len() == 1 {
                if let TypeContentToFill::SpecialType(SpecialTypeToFill::Era(era_to_fill)) =
                    &fields_to_fill[0].type_to_fill.content
                {
                    if let EraToFill::Mortal { .. } = era_to_fill {
                        era_is_immortal = false;
                    }
                    break;
                }
            }
        }
    }
    era_is_immortal
}

macro_rules! got_filled_hash {
    ($($func: ident, $hash: ident), *) => {
        $(
            fn $func(content: &mut TypeContentToFill, hash: H256) -> bool {
                if let TypeContentToFill::SpecialType(SpecialTypeToFill::H256(ref mut hash_to_fill)) =
                        content
                    {
                        if let SpecialtyH256::$hash = hash_to_fill.specialty {
                            hash_to_fill.hash = Some(hash);
                            true
                        }
                        else {false}
                    }
                else {false}
            }
        )*
    }
}

got_filled_hash!(genesis_hash_got_filled, GenesisHash);
got_filled_hash!(block_hash_got_filled, BlockHash);

struct CheckExtensions {
    found_block_hash: bool,
    found_era: bool,
    found_genesis_hash: bool,
    found_spec_version: bool,
}

impl CheckExtensions {
    fn init() -> Self {
        Self {
            found_block_hash: false,
            found_era: false,
            found_genesis_hash: false,
            found_spec_version: false,
        }
    }
    fn check_iteration(&mut self, content: &TypeContentToFill) -> Result<(), ExtensionsError> {
        match content {
            TypeContentToFill::SpecialType(SpecialTypeToFill::Era(_)) => {
                if self.found_era {
                    return Err(ExtensionsError::EraTwice);
                } else {
                    self.found_era = true;
                }
            }
            TypeContentToFill::SpecialType(SpecialTypeToFill::H256(h256_to_fill)) => {
                match h256_to_fill.specialty {
                    SpecialtyH256::BlockHash => {
                        if self.found_block_hash {
                            return Err(ExtensionsError::GenesisHashTwice);
                        } else {
                            self.found_block_hash = true;
                        }
                    }
                    SpecialtyH256::GenesisHash => {
                        if self.found_genesis_hash {
                            return Err(ExtensionsError::GenesisHashTwice);
                        } else {
                            self.found_genesis_hash = true;
                        }
                    }
                    SpecialtyH256::None => {}
                }
            }
            TypeContentToFill::Primitive(PrimitiveToFill::CompactUnsigned(
                specialty_unsigned_to_fill,
            )) => {
                if let SpecialtyUnsignedInteger::SpecVersion = specialty_unsigned_to_fill.specialty
                {
                    if self.found_spec_version {
                        return Err(ExtensionsError::SpecVersionTwice);
                    } else {
                        self.found_spec_version = true;
                    }
                }
            }
            TypeContentToFill::Primitive(PrimitiveToFill::Unsigned(specialty_unsigned_to_fill)) => {
                if let SpecialtyUnsignedInteger::SpecVersion = specialty_unsigned_to_fill.specialty
                {
                    if self.found_spec_version {
                        return Err(ExtensionsError::SpecVersionTwice);
                    } else {
                        self.found_spec_version = true;
                    }
                }
            }
            _ => {}
        }
        Ok(())
    }
}

fn check_extensions(extensions: &[TypeToFill]) -> Result<(), ExtensionsError> {
    let mut check_extensions = CheckExtensions::init();
    for ext in extensions.iter() {
        check_extensions.check_iteration(&ext.content)?;
        if let TypeContentToFill::Composite(fields_to_fill) = &ext.content {
            if fields_to_fill.len() == 1 {
                check_extensions.check_iteration(&fields_to_fill[0].type_to_fill.content)?;
            }
        }
    }
    if !check_extensions.found_genesis_hash {
        return Err(ExtensionsError::NoGenesisHash);
    }
    if !check_extensions.found_spec_version {
        return Err(ExtensionsError::NoSpecVersion);
    }
    Ok(())
}

pub fn prepare_type<E, M>(
    ty_input: &Ty,
    ext_memory: &mut E,
    registry: &M::TypeRegistry,
    mut propagated: Propagated,
) -> Result<TypeToFill, RegistryError>
where
    E: ExternalMemory,
    M: AsFillMetadata<E>,
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
        SpecialtyTypeHinted::H256 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(H256ToFill::special_to_fill(
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
        SpecialtyTypeHinted::SignatureEd25519 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(SignatureEd25519::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::SignatureSr25519 => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(SignatureSr25519::special_to_fill(
                &propagated.checker.specialty_set,
            )?),
            info: propagated.info,
        }),
        SpecialtyTypeHinted::SignatureEcdsa => Ok(TypeToFill {
            content: TypeContentToFill::SpecialType(SignatureEcdsa::special_to_fill(
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
                        }),
                        info,
                    }),
                    SequenceDraft::Regular(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::SequenceRegular(SequenceRegularToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            ty: sequence_draft_content.resolved_ty.ty,
                            id: sequence_draft_content.resolved_ty.id,
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
                            len: x.len,
                        }),
                        info,
                    }),
                    SequenceDraft::Regular(sequence_draft_content) => Ok(TypeToFill {
                        content: TypeContentToFill::ArrayRegular(ArrayRegularToFill {
                            content: Vec::new(),
                            info_element: sequence_draft_content.info_element,
                            ty: sequence_draft_content.resolved_ty.ty,
                            id: sequence_draft_content.resolved_ty.id,
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
    M: AsFillMetadata<E>,
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
    M: AsFillMetadata<E>,
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
) -> Result<BitSequenceContent, RegistryError>
where
    E: ExternalMemory,
    M: AsFillMetadata<E>,
{
    // BitOrder
    let bitorder = find_bit_order_ty::<E, M>(bit_ty, id, ext_memory, registry)?;

    // BitStore
    let bitstore_type = registry.resolve_ty(bit_ty.bit_store_type.id, ext_memory)?;

    match bitstore_type.type_def {
        TypeDef::Primitive(TypeDefPrimitive::U8) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceContent::BitVecU8Lsb0(BitVec::new())),
            FoundBitOrder::Msb0 => Ok(BitSequenceContent::BitVecU8Msb0(BitVec::new())),
        },
        TypeDef::Primitive(TypeDefPrimitive::U16) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceContent::BitVecU16Lsb0(BitVec::new())),
            FoundBitOrder::Msb0 => Ok(BitSequenceContent::BitVecU16Msb0(BitVec::new())),
        },
        TypeDef::Primitive(TypeDefPrimitive::U32) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceContent::BitVecU32Lsb0(BitVec::new())),
            FoundBitOrder::Msb0 => Ok(BitSequenceContent::BitVecU32Msb0(BitVec::new())),
        },
        #[cfg(target_pointer_width = "64")]
        TypeDef::Primitive(TypeDefPrimitive::U64) => match bitorder {
            FoundBitOrder::Lsb0 => Ok(BitSequenceContent::BitVecU64Lsb0(BitVec::new())),
            FoundBitOrder::Msb0 => Ok(BitSequenceContent::BitVecU64Msb0(BitVec::new())),
        },
        _ => Err(RegistryError::NotBitStoreType { id }),
    }
}
