use frame_metadata::{v14::RuntimeMetadataV14, v15::RuntimeMetadataV15};
use parity_scale_codec::Decode;
use primitive_types::H256;

use crate::fill_prepare::TransactionToFill;
use crate::traits::{AsFillMetadata, Unsigned};

fn metadata_v14(filename: &str) -> RuntimeMetadataV14 {
    let metadata_hex = std::fs::read_to_string(filename).unwrap();
    let metadata_vec = hex::decode(metadata_hex.trim()).unwrap();
    RuntimeMetadataV14::decode(&mut &metadata_vec[5..]).unwrap()
}

fn metadata_v15(filename: &str) -> RuntimeMetadataV15 {
    let metadata_hex = std::fs::read_to_string(filename).unwrap();
    let metadata_vec = hex::decode(metadata_hex.trim()).unwrap();
    RuntimeMetadataV15::decode(&mut &metadata_vec[5..]).unwrap()
}

#[test]
fn as_fill_metadata_1() {
    let metadata_westend = metadata_v14("for_tests/westend9111");
    let spec_version =
        <RuntimeMetadataV14 as AsFillMetadata<()>>::spec_version(&metadata_westend).unwrap();
    assert_eq!(spec_version, Unsigned::U32(9111u32));
    let tx_version =
        <RuntimeMetadataV14 as AsFillMetadata<()>>::defined_tx_version(&metadata_westend).unwrap();
    assert_eq!(tx_version, Unsigned::U32(7u32));
}

#[test]
fn as_fill_metadata_2() {
    let metadata_westend = metadata_v15("for_tests/westend1006001");
    let spec_version =
        <RuntimeMetadataV15 as AsFillMetadata<()>>::spec_version(&metadata_westend).unwrap();
    assert_eq!(spec_version, Unsigned::U32(1006001u32));
    let tx_version =
        <RuntimeMetadataV15 as AsFillMetadata<()>>::defined_tx_version(&metadata_westend).unwrap();
    assert_eq!(tx_version, Unsigned::U32(24u32));
}

#[test]
fn init_transaction() {
    let metadata_westend = metadata_v14("for_tests/westend9111");
    let genesis_hash_westend = H256(
        hex::decode("e143f23803ac50e8f6f8e62695d1ce9e4e1d68aa36c1cd2cfd15340213f3423e")
            .unwrap()
            .try_into()
            .unwrap(),
    );
    let transaction_to_fill_test =
        TransactionToFill::init(&mut (), &metadata_westend, genesis_hash_westend);
    assert!(transaction_to_fill_test.is_ok());
    let transaction_to_fill = transaction_to_fill_test.unwrap();
    assert!(transaction_to_fill.signature_is_sr25519());
}
