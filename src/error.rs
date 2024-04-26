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
    Registry(RegistryError),
    UnexpectedVariantIndex,
    UnfinalizedExtension,
    WrongExtraStructure,
}

impl<E: ExternalMemory, M: AsMetadata<E>> From<RegistryError> for ErrorFixMe<E, M> {
    fn from(registry_error: RegistryError) -> Self {
        ErrorFixMe::Registry(registry_error)
    }
}

#[derive(Debug)]
pub enum StorageRegistryError {
    MapHashesNotATuple,
    MapHashesNumberMismatch,
}
