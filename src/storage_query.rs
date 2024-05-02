use external_memory_tools::ExternalMemory;
use frame_metadata::v14::{
    PalletStorageMetadata, StorageEntryMetadata, StorageEntryType, StorageHasher,
};
use parity_scale_codec::Encode;
use scale_info::{form::PortableForm, interner::UntrackedSymbol};
use sp_crypto_hashing::{blake2_128, blake2_256, twox_128, twox_256, twox_64};
use substrate_parser::{
    cards::Documented, decoding_sci::Ty, error::RegistryError, propagated::Propagated,
};

use std::any::TypeId;

use crate::error::StorageRegistryError;
use crate::fill_prepare::{prepare_type, TypeToFill};
use crate::finalize::{Finalize, TypeContent};
use crate::traits::{AsFillMetadata, AsPalletMetadata};

#[derive(Clone, Debug)]
pub enum EntrySelector {
    Empty,
    Functional(EntrySelectorFunctional),
}

impl EntrySelector {
    pub fn init<E: ExternalMemory, M: AsFillMetadata<E>>(
        available_entries: &[StorageEntryMetadata<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<Self, RegistryError> {
        if available_entries.is_empty() {
            Ok(Self::Empty)
        } else {
            Ok(Self::Functional(EntrySelectorFunctional::new_at::<E, M>(
                available_entries,
                ext_memory,
                registry,
                0usize,
            )?))
        }
    }
}

#[derive(Clone, Debug)]
pub struct EntrySelectorFunctional {
    pub available_entries: Vec<StorageEntryMetadata<PortableForm>>,
    pub selected_entry: StorageEntry,
}

impl EntrySelectorFunctional {
    pub fn new_at<E: ExternalMemory, M: AsFillMetadata<E>>(
        available_entries: &[StorageEntryMetadata<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
        selector_index: usize,
    ) -> Result<Self, RegistryError> {
        let selected_entry_metadata = &available_entries[selector_index];
        let name = selected_entry_metadata.name.to_owned();
        let docs = selected_entry_metadata.collect_docs();
        let type_to_fill = match &selected_entry_metadata.ty {
            StorageEntryType::Plain(ty) => StorageEntryTypeToFill::Plain(*ty),
            StorageEntryType::Map {
                hashers,
                key,
                value,
            } => {
                let key_to_fill = prepare_type::<E, M>(
                    &Ty::Symbol(key),
                    ext_memory,
                    registry,
                    Propagated::new(),
                )?;
                StorageEntryTypeToFill::Map {
                    hashers: hashers.to_vec(),
                    key_to_fill,
                    value: *value,
                }
            }
        };
        Ok(Self {
            available_entries: available_entries.to_owned(),
            selected_entry: StorageEntry {
                selector_index_entry: selector_index,
                name,
                type_to_fill,
                docs,
            },
        })
    }

    pub fn selector_up<E: ExternalMemory, M: AsFillMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<(), RegistryError> {
        let new_selector_index = {
            if self.selected_entry.selector_index_entry + 1 == self.available_entries.len() {
                0
            } else {
                self.selected_entry.selector_index_entry + 1
            }
        };
        *self = EntrySelectorFunctional::new_at::<E, M>(
            &self.available_entries,
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
            if self.selected_entry.selector_index_entry == 0 {
                self.available_entries.len() - 1
            } else {
                self.selected_entry.selector_index_entry - 1
            }
        };
        *self = EntrySelectorFunctional::new_at::<E, M>(
            &self.available_entries,
            ext_memory,
            registry,
            new_selector_index,
        )?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct StorageEntry {
    pub selector_index_entry: usize,
    pub name: String,
    pub type_to_fill: StorageEntryTypeToFill,
    pub docs: String,
}

#[derive(Clone, Debug)]
pub enum StorageEntryTypeToFill {
    Plain(UntrackedSymbol<TypeId>),
    Map {
        hashers: Vec<StorageHasher>,
        key_to_fill: TypeToFill,
        value: UntrackedSymbol<TypeId>,
    },
}

#[derive(Clone, Debug)]
pub enum StorageSelector {
    Empty,
    Functional(StorageSelectorFunctional),
}

impl StorageSelector {
    pub fn init<E: ExternalMemory, M: AsFillMetadata<E>>(
        ext_memory: &mut E,
        metadata: &M,
    ) -> Result<Self, RegistryError> {
        let mut available_pallets: Vec<PalletStorageMetadata<PortableForm>> = Vec::new();
        for pallet in metadata.pallets().into_iter() {
            if let Some(pallet_storage_metadata) = pallet.storage() {
                available_pallets.push(pallet_storage_metadata)
            }
        }
        if available_pallets.is_empty() {
            Ok(Self::Empty)
        } else {
            Ok(Self::Functional(StorageSelectorFunctional::new_at::<E, M>(
                &available_pallets,
                ext_memory,
                &metadata.types(),
                0usize,
            )?))
        }
    }
}

#[derive(Clone, Debug)]
pub struct StorageSelectorFunctional {
    pub available_pallets: Vec<PalletStorageMetadata<PortableForm>>,
    pub query: StorageQuery,
}

impl StorageSelectorFunctional {
    pub fn new_at<E: ExternalMemory, M: AsFillMetadata<E>>(
        available_pallets: &[PalletStorageMetadata<PortableForm>],
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
        selector_index: usize,
    ) -> Result<Self, RegistryError> {
        // TODO case bad index
        let selected_pallet_metadata = &available_pallets[selector_index];

        let query = StorageQuery {
            selector_index_pallet: selector_index,
            prefix: selected_pallet_metadata.prefix.to_owned(),
            entry_selector: EntrySelector::init::<E, M>(
                &selected_pallet_metadata.entries,
                ext_memory,
                registry,
            )?,
        };

        Ok(Self {
            available_pallets: available_pallets.to_owned(),
            query,
        })
    }

    pub fn selector_up<E: ExternalMemory, M: AsFillMetadata<E>>(
        &mut self,
        ext_memory: &mut E,
        registry: &M::TypeRegistry,
    ) -> Result<(), RegistryError> {
        let new_selector_index = {
            if self.query.selector_index_pallet + 1 == self.available_pallets.len() {
                0
            } else {
                self.query.selector_index_pallet + 1
            }
        };
        *self = StorageSelectorFunctional::new_at::<E, M>(
            &self.available_pallets,
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
            if self.query.selector_index_pallet == 0 {
                self.available_pallets.len() - 1
            } else {
                self.query.selector_index_pallet - 1
            }
        };
        *self = StorageSelectorFunctional::new_at::<E, M>(
            &self.available_pallets,
            ext_memory,
            registry,
            new_selector_index,
        )?;
        Ok(())
    }
}

#[derive(Clone, Debug)]
pub struct StorageQuery {
    pub selector_index_pallet: usize,
    pub prefix: String,
    pub entry_selector: EntrySelector,
}

#[derive(Clone, Debug)]
pub struct FinalizedStorageQuery {
    pub key: String,
    pub value_ty: UntrackedSymbol<TypeId>,
}

impl StorageQuery {
    pub fn finalize(&self) -> Result<Option<FinalizedStorageQuery>, StorageRegistryError> {
        if let EntrySelector::Functional(entry_selector_functional) = &self.entry_selector {
            let mut key = format!(
                "0x{}{}",
                hex::encode(twox_128(self.prefix.as_bytes())),
                hex::encode(twox_128(
                    entry_selector_functional.selected_entry.name.as_bytes()
                ))
            );
            match &entry_selector_functional.selected_entry.type_to_fill {
                StorageEntryTypeToFill::Plain(value_ty) => Ok(Some(FinalizedStorageQuery {
                    key,
                    value_ty: *value_ty,
                })),
                StorageEntryTypeToFill::Map {
                    hashers,
                    key_to_fill,
                    value,
                } => {
                    if let Some(type_content) = key_to_fill.finalize() {
                        if hashers.len() == 1 {
                            key.push_str(&hex::encode(hashed_key_element(
                                &type_content.encode(),
                                &hashers[0],
                            )));
                            Ok(Some(FinalizedStorageQuery {
                                key,
                                value_ty: *value,
                            }))
                        } else if let TypeContent::Tuple(tuple_elements) = type_content {
                            if tuple_elements.len() == hashers.len() {
                                let imax = tuple_elements.len();
                                for i in 0..imax {
                                    key.push_str(&hex::encode(hashed_key_element(
                                        &tuple_elements[i].encode(),
                                        &hashers[i],
                                    )));
                                }
                                Ok(Some(FinalizedStorageQuery {
                                    key,
                                    value_ty: *value,
                                }))
                            } else {
                                Err(StorageRegistryError::MapHashesNumberMismatch)
                            }
                        } else {
                            Err(StorageRegistryError::MapHashesNotATuple)
                        }
                    } else {
                        Ok(None)
                    }
                }
            }
        } else {
            Ok(None)
        }
    }
}

pub fn hashed_key_element(data: &[u8], hasher: &StorageHasher) -> Vec<u8> {
    match hasher {
        StorageHasher::Blake2_128 => blake2_128(data).to_vec(),
        StorageHasher::Blake2_256 => blake2_256(data).to_vec(),
        StorageHasher::Blake2_128Concat => [blake2_128(data).to_vec(), data.to_vec()].concat(),
        StorageHasher::Twox128 => twox_128(data).to_vec(),
        StorageHasher::Twox256 => twox_256(data).to_vec(),
        StorageHasher::Twox64Concat => [twox_64(data).to_vec(), data.to_vec()].concat(),
        StorageHasher::Identity => data.to_vec(),
    }
}
