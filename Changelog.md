This is the changelog, summarising changes in each version(some minor changes may be ommited).

# 1.0

### 1.4.0

Added `Identity` trait

### 1.3.0

Added `type_fn` macro.

### 1.2.0

Added `"const_marker"` feature, enabled by the default feature.

Added `const_marker` module, conditional on `"const_marker"` feature, with these items: 
- `Bool` (unit struct)
- `Char` (unit struct)
- `I128` (unit struct)
- `I16` (unit struct)
- `I32` (unit struct)
- `I64` (unit struct)
- `I8` (unit struct)
- `Isize` (unit struct)
- `U128` (unit struct)
- `U16` (unit struct)
- `U32` (unit struct)
- `U64` (unit struct)
- `U8` (unit struct)
- `Usize` (unit struct)

Every type in `const_marker` defines an `eq` method, and implement `Copy + Clone + Debug`.

Added `TypeNe` type, with a `new_unchecked` constructor function.
`TypeNe` implements `Clone`, `Copy`, `Debug`, `Eq`, `Hash`, `Ord`, `PartialEq`, `PartialOrd`.

Added `TypeEq::{in_array, zip, zip3, zip4}` methods
    
Added `typewit::type_fn::FnIdentity` type-level function.



### 1.1.0

Reduced Minimum Supported Rust Version to 1.57.0

Added `"rust_stable"` and `"rust_1_61"` features

Added `type_fn::Invoke` struct, which implements `TypeFn`.

Removed blanket impl of `TypeFn` for `Fn`(minor breaking change)

### 1.0.0

Declared `TypeEq` struct, with these associated items:
- `NEW` (associated constant)
- `flip` (method)
- `in_box` (method)
- `in_mut` (method)
- `in_ref` (method)
- `join` (method)
- `map` (method)
- `new` (method)
- `new_unchecked` (method)
- `project` (method)
- `reachability_hint` (method)
- `to_left` (method)
- `to_right` (method)
`TypeEq` implements `Clone`, `Copy`, `Debug`, `Default`, `Eq`, `Hash`, `MakeTypeWitness`, `Ord`, `PartialEq`, `PartialOrd`, `TypeWitnessTypeArg`.


Declared `type_eq` function.

Declared `HasTypeWitness`, `MakeTypeWitness`, `TypeWitnessTypeArg` traits.

Declared `simple_type_witness` macro.


Declared `type_fn` module with these items:
- `TypeFn` trait
- `CallFn` type alias
- GBox (structs), this implements `TypeFn`
- GRef (structs), this implements `TypeFn`
- GRefMut (structs), this implements `TypeFn`

Declared these crate features:
- `alloc`
- `mut_refs`
- `nightly_mut_refs`

Set the Minimum Supported Rust Version to 1.61.0