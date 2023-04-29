This is the changelog, summarising changes in each version(some minor changes may be ommited).

# 0.1

### 0.1.0

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