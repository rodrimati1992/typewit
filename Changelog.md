This is the changelog, summarising changes in each version(some minor changes may be ommited).

# 1.0

### 1.6.0

Added `"nightly_const_marker"` feature.

Added `BoolWit` in `typewit::const_marker`

Added these structs in `typewit::const_marker`(under the `"nightly_const_marker"` feature):
- `Str` (unit struct)

Added these structs in `typewit::const_marker::slice`(under the `"nightly_const_marker"` feature):
- `BoolSlice` (unit struct)
- `CharSlice` (unit struct)
- `I8Slice` (unit struct)
- `I16Slice` (unit struct)
- `I32Slice` (unit struct)
- `I64Slice` (unit struct)
- `I128Slice` (unit struct)
- `IsizeSlice` (unit struct)
- `StrSlice` (unit struct)
- `U8Slice` (unit struct)
- `U16Slice` (unit struct)
- `U32Slice` (unit struct)
- `U64Slice` (unit struct)
- `U128Slice` (unit struct)
- `UsizeSlice` (unit struct)

Changed `"nightly_mut_refs*"` crate feature to enable the `"rust_stable"` feature.

### 1.5.0

Added support for these to `simple_type_witness` macro:
- Generic parameters using `< >` syntax
- Non-`[ ]`-surrounded `where` clauses
- `#[cfg(...)]` attributes on variants and generic parameters
- Using `__Wit` in constraints.
- Defaulted generic parameters (only used for defaulting generic arguments in `MakeTypeWitness` impls)

Added support for `#[cfg(...)]` attributes on generic parameters to `type_fn` macro.

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