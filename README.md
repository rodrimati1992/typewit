[![Rust](https://github.com/rodrimati1992/typewit/workflows/Rust/badge.svg)](https://github.com/rodrimati1992/typewit/actions)
[![crates-io](https://img.shields.io/crates/v/typewit.svg)](https://crates.io/crates/typewit)
[![api-docs](https://docs.rs/typewit/badge.svg)](https://docs.rs/typewit/*)


This crate provides abstractions for creating
[type witnesses](#what-are-type-witnesses).

The inciting motivation for this crate is emulating trait polymorphism in `const fn`
(as of 2023-09-10, it's not possible to call trait methods in const contexts on stable).

# What are type witnesses

Type witnesses are enums that allow coercing between a type parameter and a
range of possible types (one per variant).

The simplest type witness is [`TypeEq<L, R>`](crate::TypeEq),
which only allows coercing between `L` and `R`.

Most type witnesses are enums with [`TypeEq`] fields,
which can coerce between a type parameter and as many types as there are variants.

# Examples

<span id="example0"></span>

### Polymorphic function

This demonstrates how one can write a return-type-polymorphic `const fn`
(as of 2023-09-10, trait methods can't be called in const fns on stable)

```rust
use typewit::{MakeTypeWitness, TypeEq};

assert_eq!(returnal::<u8>(), 3);
assert_eq!(returnal::<&str>(), "hello");


const fn returnal<'a, R>() -> R
where
    RetWitness<'a, R>: MakeTypeWitness,
{
    match MakeTypeWitness::MAKE {
        RetWitness::U8(te) => {
            // `te` (a `TypeEq<R, u8>`) allows coercing between `R` and `u8`,
            // because `TypeEq` is a value-level proof that both types are the same.
            // `te.to_left(...)` goes from `u8` to `R`.
            te.to_left(3u8)
        }
        RetWitness::Str(te) => {
            // `te` is a `TypeEq<R, &'a str>`
            // `te.to_left(...)` goes from `&'a str` to `R`.
            te.to_left("hello")
        }
    }
}

// This macro declares a type witness enum
typewit::simple_type_witness! {
    // Declares `enum RetWitness<'a, __Wit>` 
    // (the `__Wit` type parameter is implicitly added after all generics)
    enum RetWitness<'a> {
        // This variant requires `__Wit == u8`
        U8 = u8,
   
        // This variant requires `__Wit == &'a str`
        Str = &'a str,
    }
}
```

<span id="example1"></span>
### Indexing polymorphism

This function demonstrates const fn polymorphism
and projecting [`TypeEq`] by implementing [`TypeFn`].

(this example requires Rust 1.71.0, because it uses `<[T]>::split_at` in a const context.
```rust
use std::ops::Range;

use typewit::{HasTypeWitness, TypeEq};

fn main() {
    let array = [3, 5, 8, 13, 21, 34, 55, 89];

    assert_eq!(index(&array, 0), &3);
    assert_eq!(index(&array, 3), &13);
    assert_eq!(index(&array, 0..4), [3, 5, 8, 13]);
    assert_eq!(index(&array, 3..5), [13, 21]);
}

const fn index<T, I>(slice: &[T], idx: I) -> &SliceIndexRet<I, T>
where
    I: SliceIndex<T>,
{
    // `I::WITNESS` is `<I as HasTypeWitness<IndexWitness<I>>>::WITNESS`,
    match I::WITNESS {
        IndexWitness::Usize(arg_te) => {
            // `arg_te` (a `TypeEq<I, usize>`) allows coercing between `I` and `usize`,
            // because `TypeEq` is a value-level proof that both types are the same.
            let idx: usize = arg_te.to_right(idx);

            // using the `TypeFn` impl for `FnSliceIndexRet<T>` to 
            // map `TypeEq<I, usize>` 
            // to  `TypeEq<SliceIndexRet<I, T>, SliceIndexRet<usize, T>>`
            arg_te.project::<FnSliceIndexRet<T>>()
                // converts`TypeEq<SliceIndexRet<I, T>, T>` 
                //      to `TypeEq<&SliceIndexRet<I, T>, &T>`
                .in_ref()
                .to_left(&slice[idx])
        }
        IndexWitness::Range(arg_te) => {
            let range: Range<usize> = arg_te.to_right(idx);
            let ret: &[T] = slice_range(slice, range);
            arg_te.project::<FnSliceIndexRet<T>>().in_ref().to_left(ret)
        }
    }
}

// This macro declares a type witness enum
typewit::simple_type_witness! {
    // Declares `enum IndexWitness<__Wit>` 
    // (the `__Wit` type parameter is implicitly added after all generics)
    enum IndexWitness {
        // This variant requires `__Wit == usize`
        Usize = usize,
   
        // This variant requires `__Wit == Range<usize>`
        Range = Range<usize>,
    }
}

/// Trait for all types that can be used as slice indices
/// 
/// The `HasTypeWitness` supertrait allows getting a `IndexWitness<Self>`
/// with its `WITNESS` associated constant.
trait SliceIndex<T>: HasTypeWitness<IndexWitness<Self>> + Sized {
    type Returns: ?Sized;
}
impl<T> SliceIndex<T> for usize {
    type Returns = T;
}
impl<T> SliceIndex<T> for Range<usize> {
    type Returns = [T];
}

type SliceIndexRet<I, T> = <I as SliceIndex<T>>::Returns;

// Declares `struct FnSliceIndexRet<T>`
// a type-level function (TypeFn implementor) from `I` to `SliceIndexRet<I, T>`
typewit::type_fn! {
    struct FnSliceIndexRet<T>;

    impl<I: SliceIndex<T>> I => SliceIndexRet<I, T>
}

const fn slice_range<T>(slice: &[T], range: Range<usize>) -> &[T] {
    let suffix = slice.split_at(range.start).1;
    suffix.split_at(range.end - range.start).0
}

```

When the wrong type is passed for the index,
the compile-time error is the same as with normal generic functions:
```text
error[E0277]: the trait bound `RangeFull: SliceIndex<{integer}>` is not satisfied
  --> src/main.rs:43:30
   |
13 |     assert_eq!(index(&array, ..), [13, 21]);
   |                -----         ^^ the trait `SliceIndex<{integer}>` is not implemented for `RangeFull`
   |                |
   |                required by a bound introduced by this call
   |
   = help: the following other types implement trait `SliceIndex<T>`:
             std::ops::Range<usize>
             usize
```

### Downcasting const generic type

This example demonstrates "downcasting" from a type with a const parameter to 
a concrete instance of that type.

This example requires the `"const_marker"` feature (enabled by default).

```rust
use typewit::{const_marker::Usize, TypeEq};

assert_eq!(*mutate(&mut Arr([])), Arr([]));
assert_eq!(*mutate(&mut Arr([1])), Arr([1]));
assert_eq!(*mutate(&mut Arr([1, 2])), Arr([1, 2]));
assert_eq!(*mutate(&mut Arr([1, 2, 3])), Arr([1, 3, 6])); // this is different!
assert_eq!(*mutate(&mut Arr([1, 2, 3, 4])), Arr([1, 2, 3, 4])); 

#[derive(Debug, PartialEq)]
struct Arr<const N: usize>([u8; N]);

fn mutate<const N: usize>(arr: &mut Arr<N>) -> &mut Arr<N> {
    if let Ok(te) =  Usize::<N>.eq(Usize::<3>) {
        let tem = te // `te` is a `TypeEq<Usize<N>, Usize<3>>`
            .project::<GArr>() // returns `TypeEq<Arr<N>, Arr<3>>`
            .in_mut(); // returns `TypeEq<&mut Arr<N>, &mut Arr<3>>`

        // `tem.to_right(arr)` downcasts `arr` to `&mut Arr<3>`
        tetra_sum(tem.to_right(arr));
    }

    arr
}

fn tetra_sum(arr: &mut Arr<3>) {
    arr.0[1] += arr.0[0];
    arr.0[2] += arr.0[1];
}

// Declares `struct GArr`
// a type-level function (TypeFn implementor) from `Usize<N>` to `Arr<N>`
typewit::type_fn!{
    struct GArr;

    impl<const N: usize> Usize<N> => Arr<N>
}
```
### Builder

Using a type witness to help encode a type-level enum,
and to match on that type-level enum inside of a function.

The type-level enum is used to track the initialization of fields in a builder.

This example requires Rust 1.65.0, because it uses Generic Associated Types.
```rust
use typewit::HasTypeWitness;

fn main() {
    // all default fields
    assert_eq!(
        StructBuilder::new().build(), 
        Struct{foo: "default value".into(), bar: vec![3, 5, 8]},
    );

    // defaulted bar field
    assert_eq!(
        StructBuilder::new().foo("hello").build(), 
        Struct{foo: "hello".into(), bar: vec![3, 5, 8]},
    );

    // defaulted foo field
    assert_eq!(
        StructBuilder::new().bar([13, 21, 34]).build(), 
        Struct{foo: "default value".into(), bar: vec![13, 21, 34]},
    );

    // all initialized fields
    assert_eq!(
        StructBuilder::new().foo("world").bar([55, 89]).build(), 
        Struct{foo: "world".into(), bar: vec![55, 89]},
    );
}


#[derive(Debug, PartialEq, Eq)]
struct Struct {
    foo: String,
    bar: Vec<u32>,
}

struct StructBuilder<FooInit: InitState, BarInit: InitState> {
    // If `FooInit` is `Uninit`, then this field is a `()`
    // If `FooInit` is `Init`, then this field is a `String`
    foo: BuilderField<FooInit, String>,

    // If `BarInit` is `Uninit`, then this field is a `()`
    // If `BarInit` is `Init`, then this field is a `Vec<u32>`
    bar: BuilderField<BarInit, Vec<u32>>,
}

impl StructBuilder<Uninit, Uninit> {
    pub const fn new() -> Self {
        Self {
            foo: (),
            bar: (),
        }
    }
}

impl<FooInit: InitState, BarInit: InitState> StructBuilder<FooInit, BarInit> {
    /// Sets the `foo` field
    pub fn foo(self, foo: impl Into<String>) -> StructBuilder<Init, BarInit> {
        StructBuilder {
            foo: foo.into(),
            bar: self.bar,
        }
    }

    /// Sets the `bar` field
    pub fn bar(self, bar: impl Into<Vec<u32>>) -> StructBuilder<FooInit, Init> {
        StructBuilder {
            foo: self.foo,
            bar: bar.into(),
        }
    }

    /// Builds `Struct`, 
    /// providing default values for fields that haven't been set.
    pub fn build(self) -> Struct {
        typewit::type_fn! {
            struct HelperFn<T>;
            impl<I: InitState> I => BuilderField<I, T>
        }

        Struct {
            // matching on the type-level `InitState` enum by using `InitWit`.
            // `WITNESS` comes from the `HasTypeWitness` trait
            foo: match FooInit::WITNESS {
                // `te: TypeEq<FooInit, Init>`
                InitWit::InitW(te) => {
                    te.map(HelperFn::NEW) //: TypeEq<BuilderField<FooInit, String>, String>
                      .to_right(self.foo)
                }
                InitWit::UninitW(_) => "default value".to_string(),
            },
            bar: match BarInit::WITNESS {
                // `te: TypeEq<BarInit, Init>`
                InitWit::InitW(te) => {
                    te.map(HelperFn::NEW) //: TypeEq<BuilderField<BarInit, Vec<u32>>, Vec<u32>>
                      .to_right(self.bar)
                }
                InitWit::UninitW(_) => vec![3, 5, 8],
            },
        }
    }
}

// Emulates a type-level `enum InitState { Init, Uninit }`
trait InitState: Sized + HasTypeWitness<InitWit<Self>> {
    // How a builder represents an initialized/uninitialized field.
    // If `Self` is `Uninit`, then this is `()`.
    // If `Self` is `Init`, then this is `T`.
    type BuilderField<T>;
}

// If `I` is `Uninit`, then this evaluates to `()`
// If `I` is `Init`, then this evaluates to `T`
type BuilderField<I, T> = <I as InitState>::BuilderField::<T>;

// Emulates a type-level `InitState::Init` variant.
// Marks a field as initialized.
enum Init {}

impl InitState for Init {
    type BuilderField<T> = T;
}

// Emulates a type-level `InitState::Uninit` variant
// Marks a field as uninitialized.
enum Uninit {}

impl InitState for Uninit {
    type BuilderField<T> = ();
}

typewit::simple_type_witness! {
    // Declares `enum InitWit<__Wit>`, a type witness.
    // (the `__Wit` type parameter is implicitly added after all generics)
    enum InitWit {
        // This variant requires `__Wit == Init`
        InitW = Init,
        // This variant requires `__Wit == Uninit`
        UninitW = Uninit,
    }
}
```


# Cargo features

These are the features of this crates:

- `"rust_1_61"`: allows the `typewit` crate to use Rust 1.61.0 features.

- `"rust_stable"`: enables all the `"rust_1_*"` features.

- `"alloc"`: enable items that use anything from the standard `alloc` crate.

- `"const_marker"`(enabled by default): enables the [`const_marker`] module,
the `"cmp"` crate feature,
and all items that depend on it.

- `"cmp"`(enabled by default): enables the [`TypeCmp`] type.

- `"inj_type_fn"`(enabled by default): 
Enables `type_fn::{CallInjFn, FnRev, InjTypeFn, RevTypeFn, UncallFn}`,
[`TypeEq`]`::{unmap, unproject}`, 
and all [`TypeNe`] functions that project its type arguments.

- `"adt_const_marker"`(requires the nightly compiler):
enables the `"rust_stable"` and `"const_marker"` crate features,
and marker types in the [`const_marker`] module that have
non-primitive `const` parameters.

- `"mut_refs"`: turns functions that take mutable references into const fns.
note: as of September 2023, 
this crate feature requires a stable compiler from the future.

- `"nightly_mut_refs"`(requires the nightly compiler):
Enables the `"rust_stable"` and `"mut_refs"` crate features,
and the `const_mut_refs` nightly feature.

None of the crate features are enabled by default.

# No-std support

`typewit` is `#![no_std]`, it can be used anywhere Rust can be used.
You need to enable the `"alloc"` feature to enable items that use anything 
from the standard `alloc` crate.

# Minimum Supported Rust Version

`typewit` supports Rust 1.57.0.

Features that require newer versions of Rust, or the nightly compiler,
need to be explicitly enabled with crate features.






[`TypeCmp`]: https://docs.rs/typewit/latest/typewit/enum.TypeCmp.html
[`TypeEq`]: https://docs.rs/typewit/latest/typewit/struct.TypeEq.html
[`TypeNe`]: https://docs.rs/typewit/latest/typewit/struct.TypeEq.html
[`TypeFn`]: https://docs.rs/typewit/latest/typewit/type_fn/trait.TypeFn.html
[`const_marker`]: https://docs.rs/typewit/latest/typewit/const_marker/index.html
