# substrate-constructor

This is a counterpart for [substrate-parser](https://github.com/Alzymologist/substrate-parser) that allows construction of extrinsics based on metadata supplied. See an example of its use in [Lempi project](https://github.com/Alzymologist/Lempi)

## Why not `subxt`?

`substrate-parser` + `substrate-constructor` system is

- much leaner (does not pull all multitude of sp-* crates, only minimal ones) - thus is more portable,
- is more modular,
- designed for clients and visualization,
- has more ideomatic extrinsic construction flow.

