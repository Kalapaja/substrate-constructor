use external_memory_tools::ExternalMemory;
use std::fmt::{Debug, Display, Formatter, Result};
use substrate_parser::{
    error::{ExtensionsError, RegistryError},
    traits::AsMetadata,
};
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ErrorFixMe<E: ExternalMemory, M: AsMetadata<E>> {
    ExtensionsList(ExtensionsError),
    ExtraNotInExtensions,
    MetaStructure(M::MetaStructureError),
    Registry(#[from] RegistryError<E>),
    UnexpectedVariantIndex,
    UnfinalizedExtension,
    WrongExtraStructure,
}

// TODO: provide actual error descriptions.
impl<E: ExternalMemory, M: AsMetadata<E>> Display for ErrorFixMe<E, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        <Self as Debug>::fmt(self, f)
    }
}

#[derive(Debug, Error)]
pub enum StorageRegistryError {
    MapHashesNotATuple,
    MapHashesNumberMismatch,
}

// TODO: provide actual error descriptions.
impl Display for StorageRegistryError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        <Self as Debug>::fmt(self, f)
    }
}
