[![Rust](https://github.com/rodrimati1992/typewit/workflows/Rust/badge.svg)](https://github.com/rodrimati1992/typewit/actions)
[![crates-io](https://img.shields.io/crates/v/typewit.svg)](https://crates.io/crates/typewit)
[![api-docs](https://docs.rs/typewit/badge.svg)](https://docs.rs/typewit/*)


This crate provides abstractions for creating type witnesses.

The inciting motivation for this crate is emulating trait polymorphism in `const fn`
(as of 2023-04-30 it's not possible to call trait methods in const contexts on stable).

# What are type witnesses

Type witnesses are enums that allow coercing between a type parameter and a
range of possible types (one per variant).

The simplest type witness is [`TypeEq<L, R>`](crate::TypeEq),
which only allows coercing between `L` and `R`.

Most type witnesses are enums with [`TypeEq`] fields,
which can coerce between a type parameter and as many types as there are variants.

# Examples

<span id="example0"></span>
### Indexing polymorphism

This example demonstrates how one can emulate trait polymorphism in `const fn`s

```rust
use std::ops::Range;

use typewit::{HasTypeWitness, MakeTypeWitness, TypeWitnessTypeArg, TypeEq};

fn main() {
    let array = [3, 5, 8, 13, 21, 34, 55, 89];

    assert_eq!(index(&array, 0), &3);
    assert_eq!(index(&array, 3), &13);
    assert_eq!(index(&array, 0..4), [3, 5, 8, 13]);
    assert_eq!(index(&array, 3..5), [13, 21]);
}

const fn index<T, I>(slice: &[T], idx: I) -> &I::Returns
where
    I: SliceIndex<T>,
{
    // `WITNESS` comes from the `HasTypeWitness` trait
    match I::WITNESS {
        IndexWitness::Usize(arg_te) => {
            // `arg_te` (a `TypeEq<I, usize>`) allows coercing between `I` and `usize`,
            // because `TypeEq` is a value-level proof that both types are the same.
            let idx: usize = arg_te.to_right(idx);

            // mapping the `TypeEq` from the argument's type to the return type.
            let ret_te: TypeEq<I::Returns, T> = project_ret(arg_te);
            // `.in_ref()` converts `TypeEq<L, R>` into `TypeEq<&L, &R>`
            let ret_te: TypeEq<&I::Returns, &T> = ret_te.in_ref();

            let ret: &T = &slice[idx];

            ret_te.to_left(ret)
        }
        IndexWitness::Range(arg_te) => {
            let range: Range<usize> = arg_te.to_right(idx);

            let ret_te: TypeEq<I::Returns, [T]> = project_ret(arg_te);
            let ret: &[T] = slice_range(slice, range);
            ret_te.in_ref().to_left(ret)
        }
    }
}

/// Trait for all types that can be used as slice indices
trait SliceIndex<T>: HasTypeWitness<IndexWitness<Self>> + Sized {
    type Returns: ?Sized;
}

// This is a type witness
enum IndexWitness<I> {
    // This variant requires `I == usize`
    Usize(TypeEq<I, usize>),

    // This variant requires `I == Range<usize>`
    Range(TypeEq<I, Range<usize>>),
}

impl<I> TypeWitnessTypeArg for IndexWitness<I> {
    type Arg = I;
}

//////

impl<T> SliceIndex<T> for usize {
    type Returns = T;
}

impl MakeTypeWitness for IndexWitness<usize> {
    const MAKE: Self = Self::Usize(TypeEq::NEW);
}

//////

impl<T> SliceIndex<T> for Range<usize> {
    type Returns = [T];
}

impl MakeTypeWitness for IndexWitness<Range<usize>> {
    const MAKE: Self = Self::Range(TypeEq::NEW);
}

//////

const fn project_ret<L, R, T>(te: TypeEq<L, R>) -> TypeEq<L::Returns, R::Returns>
where
    L: SliceIndex<T>,
    R: SliceIndex<T>,
{
    // using `SliceIndexRets`'s `TypeFn` impl to map the `TypeEq`
    te.project::<SliceIndexRets<T>>()
}

// This is a type-level function from `I` to `<I as SliceIndex<T>>::Returns`
struct SliceIndexRets<T>(std::marker::PhantomData<fn() -> T>);

// What makes SliceIndexRets a type-level function
impl<T, I: SliceIndex<T>> typewit::TypeFn<I> for SliceIndexRets<T>  {
    type Output = I::Returns;
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

# Cargo features

These are the features of these crates:

**none yet**


# No-std support

`typewit` is `#![no_std]`, it can be used anywhere Rust can be used.

# Minimum Supported Rust Version

`typewit` requires Rust 1.57.0.

Features that require newer versions of Rust, or the nightly compiler,
need to be explicitly enabled with crate features.
