use external_memory_tools::ExternalMemory;
use substrate_parser::{
    error::{ExtensionsError, RegistryError},
    traits::AsMetadata,
};

#[derive(Debug)]
pub enum ErrorFixMe<E: ExternalMemory, M: AsMetadata<E>> {
    ExtensionsList(ExtensionsError),
    ExtraNotInExtensions,
    MetaStructure(M::MetaStructureError),
    Registry(RegistryError<E>),
    UnexpectedVariantIndex,
    UnfinalizedExtension,
    WrongExtraStructure,
}

impl<E: ExternalMemory, M: AsMetadata<E>> From<RegistryError<E>> for ErrorFixMe<E, M> {
    fn from(registry_error: RegistryError<E>) -> Self {
        ErrorFixMe::Registry(registry_error)
    }
}

#[derive(Debug)]
pub enum StorageRegistryError {
    MapHashesNotATuple,
    MapHashesNumberMismatch,
}
