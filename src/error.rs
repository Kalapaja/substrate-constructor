use external_memory_tools::ExternalMemory;
use std::fmt::{Debug, Display, Formatter, Result as FmtResult};
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
    UnfinalizedExtension,
    WrongExtraStructure,
}

#[derive(Debug)]
pub enum StorageRegistryError {
    MapHashesNotATuple,
    MapHashesNumberMismatch,
}

impl StorageRegistryError {
    fn error_text(&self) -> String {
        match self {
            StorageRegistryError::MapHashesNotATuple => String::from("Unexpected storage structure in metadata. `StorageEntryType::Map { .. }` must have either a single describing type, or a set of types in tuple format."),
            StorageRegistryError::MapHashesNumberMismatch => String::from("Unexpected storage structure in metadata. `StorageEntryType::Map { .. }` has different number of hashers in hasher set and types in descriptor."),
        }
    }
}

impl Display for StorageRegistryError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.error_text())
    }
}

impl<E: ExternalMemory, M: AsMetadata<E>> ErrorFixMe<E, M> {
    fn error_text(&self) -> String {
        match self {
            ErrorFixMe::ExtensionsList(extensions_error) => format!("Unexpected extensions structure in metadata. {extensions_error}"),
            ErrorFixMe::ExtraNotInExtensions => String::from("Unexpected metadata structure. Data required to build a technical part of a sendable transaction (`extra`) is not a part of extensions set."),
            ErrorFixMe::MetaStructure(meta_structure_error) => format!("Unexpected metadata structure. {meta_structure_error}"),
            ErrorFixMe::Registry(registry_error) => format!("Error in metadata types registry. {}", registry_error),
            ErrorFixMe::UnfinalizedExtension => String::from("Unable to sign uncheched extrinsic, as some of the extensions are not completed."),
            ErrorFixMe::WrongExtraStructure => String::from("Unexpected metadata structure. Extra set is expected to be either a struct or a tuple, and apparently is neither."),
        }
    }
}

impl<E: ExternalMemory, M: AsMetadata<E>> Display for ErrorFixMe<E, M> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.error_text())
    }
}

impl<E: ExternalMemory, M: AsMetadata<E>> std::error::Error for ErrorFixMe<E, M> {}

impl std::error::Error for StorageRegistryError {}

impl<E: ExternalMemory, M: AsMetadata<E>> From<RegistryError<E>> for ErrorFixMe<E, M> {
    fn from(registry_error: RegistryError<E>) -> Self {
        ErrorFixMe::Registry(registry_error)
    }
}
