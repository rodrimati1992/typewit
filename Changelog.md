This is the changelog, summarising changes in each version(some minor changes may be ommited).

# 1.0

### 1.12.0

Added `"generic_const_exprs"` crate feature, currently only used for doc examples.

Fixed outdated README feature section.

### 1.11.0

Added `"rust_1_83"` feature, which turns `typewit` functions that use `&mut` into const fns.

Added these methods to `BoolWitG`: 
- `is_true`
- `is_false`
- `to_true`
- `to_false`
- `unwrap_true`
- `unwrap_false`

Relaxed `Copy + Clone + Debug` impls of `BooleanWitG` to work for any `<B> BooleanWitG<B>`, instead of requiring `<const B: bool> BoolWitG<Bool<B>>`.


### 1.10.1

Fixed `TypeWitnessTypeArg` impl for `BoolWitG`, it was overconstrained in a way that made `HasTypeWitness<BoolWitG<T>>` not work as a bound.

### 1.10.0

Added `typewit::const_marker::BoolWitG` enum

Replaced `typewit::const_marker::BoolWit` enum with type alias to `BoolWitG``

Added `Copy + Clone + Debug` impls to `BoolWit`

Fixed `"adt_const_marker"` crate feature

### 1.9.0

Deprecated `{TypeCmp, TypeNe}::with_any` due to unsoundness: both constructors rely on `TypeId::of::<L>() != TypeId::of::<R>()` implying `L != R`, which is not true in the general case.

### 1.8.0

Added `"rust_1_65"` feature

Added `BaseTypeWitness` trait, which requires `"rust_1_61"` feature.

Added `MetaBaseTypeWit` enum, which requires `"rust_1_61"` feature.


Added these items to `type_fn` module:
- `InjTypeFn` trait
- `RevTypeFn` trait
- `FnRev` type-level function
- `CallInjFn` type alias
- `UncallFn` type alias

Added `inj_type_fn` macro

Added reexport of these in `type_fn` module:
- `inj_type_fn` macro
- `type_fn` macro


Added `TypeCmp` enum

Added these associated items to `TypeEq`: 
- `with_any` constructor
- `to_cmp` method
- `unmap` method
- `unproject` method

Added `type_ne` macro

Added these associated items to `TypeNe`: 
- `with_any` constructor
- `with_fn` constructor
- `to_cmp` method
- `flip` method
- `join_left` method
- `join_right` method
- `map` method
- `project` method
- `unmap` method
- `unproject` method
- `in_mut` method
- `in_ref` method
- `in_array` method (requires `"rust_1_61"` feature)
- `zip` method (requires `"rust_1_61"` feature)
- `zip3` method (requires `"rust_1_61"` feature)
- `zip4` method (requires `"rust_1_61"` feature)

Removed the need to enable `"const_marker"` feature to enable `const_marker` module.

Added `equals` method to all marker types in `const_marker` module.

Deprecated `eq` methods of all marker types in `const_marker` module.

Added `type_constructors` module, which requires `"rust_1_65"` and includes these items:
- `BaseTypeWitnessTc` trait
- `TcTypeCmp` struct
- `TcTypeEq` struct
- `TcTypeNe` struct
- `BaseTypeWitnessReparam` type alias
- `BaseTypeWitnessToTc` type alias
- `MapBaseTypeWitness` type alias
- `TcToBaseTypeWitness` type alias

Added `methods` module, which requires `"rust_1_65"` and includes these items:
- `zipping` module
- `in_array` function
- `zip2` function
- `zip3` function
- `zip4` function

The `methods::zipping` submodule (which requires `"rust_1_65"`) contains:
- `Zip2` trait
- `Zip3` trait
- `Zip4` trait
- `Zip2Out` type alias
- `Zip3Out` type alias
- `Zip4Out` type alias

Added `type_ne` module, which contains:
- reexport of `TypeNe`
- reexport of `type_ne` macro
- `LeftArg` marker type
- `RightArg` marker type

Added `typewit_proc_macros` optional dependency.

Added `"proc_macros"` feature, enabled by default, which enables `typewit_proc_macros` dependency.


### 1.7.0

Added `polymatch` macro.

### 1.6.0

Added `"adt_const_marker"` feature.

Added `BoolWit` in `typewit::const_marker`

Added these structs in `typewit::const_marker`(under the `"adt_const_marker"` feature):
- `Str` (unit struct)

Added these structs in `typewit::const_marker::slice`(under the `"adt_const_marker"` feature):
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
