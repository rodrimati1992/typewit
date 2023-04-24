#![allow(clippy::needless_doctest_main)]
//! This crate provides abstractions for creating type witnesses.
//! 
//! The inciting motivation for this crate is emulating trait polymorphism in `const fn`
//! (as of 2023-04-30 it's not possible to call trait methods in const contexts on stable).
//! 
//! # What are type witnesses
//! 
//! Type witnesses are enums that allow coercing between a type parameter and a
//! range of possible types (one per variant).
//! 
//! The simplest type witness is [`TypeEq<L, R>`](crate::TypeEq),
//! which only allows coercing between `L` and `R`.
//! 
//! Most type witnesses are enums with [`TypeEq`] fields,
//! which can coerce between a type parameter and as many types as there are variants.
//! 
//! # Examples
//! 
//! <span id="example0"></span>
//! 
//! ### Polymorphic function
//! 
//! This demonstrates how one can write a return-type-polymorphic `const fn`
//! (as of 2023-04-30, trait methods can't be called in const fns)
//! 
//! ```rust
//! use typewit::{HasTypeWitness, MakeTypeWitness, TypeWitnessTypeArg, TypeEq};
//! 
//! assert_eq!(returnal::<u8>(), 3);
//! assert_eq!(returnal::<&str>(), "hello");
//! 
//! 
//! const fn returnal<'a, R>() -> R
//! where
//!     R: HasTypeWitness<RetWitness<'a, R>>
//! {
//!     // `R::WITNESS` expands to
//!     // `<R as HasTypeWitness<RetWitness<'a, R>>>::WITNESS`.
//!     // `HasTypeWitness` delegates to the `MakeTypeWitness` trait to get `WITNESS`.
//!     match R::WITNESS {
//!         RetWitness::U8(te) => {
//!             // `te` (a `TypeEq<R, u8>`) allows coercing between `R` and `u8`,
//!             // because `TypeEq` is a value-level proof that both types are the same.
//!             // `te.to_left(...)` goes from `u8` to `R`.
//!             te.to_left(3u8)
//!         }
//!         RetWitness::Str(te) => {
//!             // `te` is a `TypeEq<&'a str, R>`
//!             // `te.to_right(...)` goes from `&'a str` to `R`.
//!             te.to_right("hello")
//!         }
//!     }
//! }
//! 
//! // This is a type witness
//! enum RetWitness<'a, R> {
//!     // This variant requires `R == u8`
//!     U8(TypeEq<R, u8>),
//!
//!     // This variant requires `&'a str == R`
//!     Str(TypeEq<&'a str, R>),
//! }
//! 
//! impl<R> TypeWitnessTypeArg for RetWitness<'_, R> {
//!     type Arg = R;
//! }
//! 
//! impl MakeTypeWitness for RetWitness<'_, u8> {
//!     const MAKE: Self = RetWitness::U8(TypeEq::NEW);
//! }
//! 
//! impl<'a> MakeTypeWitness for RetWitness<'a, &'a str> {
//!     const MAKE: Self = RetWitness::Str(TypeEq::NEW);
//! }
//! 
//! ```
//! 
//! <span id="example1"></span>
//! ### Indexing polymorphism
//! 
//! ```rust
//! use std::ops::Range;
//! 
//! use typewit::{HasTypeWitness, MakeTypeWitness, TypeWitnessTypeArg, TypeEq};
//! 
//! fn main() {
//!     let array = [3, 5, 8, 13, 21, 34, 55, 89];
//! 
//!     assert_eq!(index(&array, 0), &3);
//!     assert_eq!(index(&array, 3), &13);
//!     assert_eq!(index(&array, 0..4), [3, 5, 8, 13]);
//!     assert_eq!(index(&array, 3..5), [13, 21]);
//! }
//! 
//! const fn index<T, I>(slice: &[T], idx: I) -> &I::Returns
//! where
//!     I: SliceIndex<T>,
//! {
//!     // `I::WITNESS` expands to `<I as HasTypeWitness<IndexWitness<I>>>::WITNESS`,
//!     match I::WITNESS {
//!         IndexWitness::Usize(arg_te) => {
//!             // `arg_te` (a `TypeEq<I, usize>`) allows coercing between `I` and `usize`,
//!             // because `TypeEq` is a value-level proof that both types are the same.
//!             let idx: usize = arg_te.to_right(idx);
//! 
//!             // mapping the `TypeEq` from the argument's type to the return type.
//!             let ret_te: TypeEq<I::Returns, T> = project_ret(arg_te);
//!             // `.in_ref()` converts `TypeEq<L, R>` into `TypeEq<&L, &R>`
//!             let ret_te: TypeEq<&I::Returns, &T> = ret_te.in_ref();
//! 
//!             let ret: &T = &slice[idx];
//! 
//!             ret_te.to_left(ret)
//!         }
//!         IndexWitness::Range(arg_te) => {
//!             let range: Range<usize> = arg_te.to_right(idx);
//! 
//!             let ret_te: TypeEq<I::Returns, [T]> = project_ret(arg_te);
//!             let ret: &[T] = slice_range(slice, range);
//!             ret_te.in_ref().to_left(ret)
//!         }
//!     }
//! }
//! 
//! /// Trait for all types that can be used as slice indices
//! trait SliceIndex<T>: HasTypeWitness<IndexWitness<Self>> + Sized {
//!     type Returns: ?Sized;
//! }
//! 
//! // This is a type witness
//! enum IndexWitness<I> {
//!     // This variant requires `I == usize`
//!     Usize(TypeEq<I, usize>),
//!
//!     // This variant requires `I == Range<usize>`
//!     Range(TypeEq<I, Range<usize>>),
//! }
//! 
//! impl<I> TypeWitnessTypeArg for IndexWitness<I> {
//!     type Arg = I;
//! }
//! 
//! //////
//! 
//! impl<T> SliceIndex<T> for usize {
//!     type Returns = T;
//! }
//! 
//! impl MakeTypeWitness for IndexWitness<usize> {
//!     const MAKE: Self = Self::Usize(TypeEq::NEW);
//! }
//! 
//! //////
//! 
//! impl<T> SliceIndex<T> for Range<usize> {
//!     type Returns = [T];
//! }
//! 
//! impl MakeTypeWitness for IndexWitness<Range<usize>> {
//!     const MAKE: Self = Self::Range(TypeEq::NEW);
//! }
//! 
//! //////////////////////////////
//! // stuff that maps TypeEq's type arguments
//! 
//! const fn project_ret<L, R, T>(te: TypeEq<L, R>) -> TypeEq<L::Returns, R::Returns>
//! where
//!     L: SliceIndex<T>,
//!     R: SliceIndex<T>,
//! {
//!     // using `SliceIndexRets`'s `TypeFn` impl to map the `TypeEq`
//!     te.project::<SliceIndexRets<T>>()
//! }
//! 
//! // This is a type-level function from `I` to `<I as SliceIndex<T>>::Returns`
//! struct SliceIndexRets<T>(std::marker::PhantomData<fn() -> T>);
//! 
//! // What makes SliceIndexRets a type-level function
//! impl<T, I: SliceIndex<T>> typewit::TypeFn<I> for SliceIndexRets<T>  {
//!     type Output = I::Returns;
//! }
//! 
//! # // would use `konst::slice::slice_range`,
//! # // but it would become a cyclic dependency once `konst` depends on this crate.
//! # const fn slice_range<T>(mut slice: &[T], Range{mut start, end}: Range<usize>) -> &[T] {
//! #     assert!(start <= end && end <= slice.len());
//! #     let mut removed_end = slice.len() - end;
//! #     while let ([_, rem @ ..], 1..) = (slice, start) {
//! #         start -= 1;
//! #         slice = rem;
//! #     }
//! #     while let ([rem @ .., _], 1..) = (slice, removed_end) {
//! #         removed_end -= 1;
//! #         slice = rem;
//! #     }
//! #     slice
//! # }
//! ```
//! 
//! When the wrong type is passed for the index,
//! the compile-time error is the same as with normal generic functions:
//! ```text
//! error[E0277]: the trait bound `RangeFull: SliceIndex<{integer}>` is not satisfied
//!   --> src/main.rs:43:30
//!    |
//! 13 |     assert_eq!(index(&array, ..), [13, 21]);
//!    |                -----         ^^ the trait `SliceIndex<{integer}>` is not implemented for `RangeFull`
//!    |                |
//!    |                required by a bound introduced by this call
//!    |
//!    = help: the following other types implement trait `SliceIndex<T>`:
//!              std::ops::Range<usize>
//!              usize
//! ```
//! 
//! # Cargo features
//! 
//! These are the features of this crates:
//! 
//! - `"alloc"`: enable items that use anything from the `alloc` crate.
//! 
//! - `"mut_refs"`: turns functions that take mutable references into const fns.
//! note: as of April 2023, 
//! this crate feature requires a stable compiler from the future.
//! 
//! - `"nightly_mut_refs"`(requires the nightly compiler):
//! Enables the `"mut_refs"` crate feature and 
//! the `const_mut_refs` nightly feature.
//! 
//! 
//! # No-std support
//! 
//! `typewit` is `#![no_std]`, it can be used anywhere Rust can be used.
//! 
//! # Minimum Supported Rust Version
//! 
//! `typewit` requires Rust 1.61.0.
//! 
//! Features that require newer versions of Rust, or the nightly compiler,
//! need to be explicitly enabled with crate features.
//! 
//! 
//! 
//! [`TypeEq`]: crate::TypeEq
#![no_std]
#![cfg_attr(feature = "nightly_mut_refs", feature(const_mut_refs))]
#![cfg_attr(feature = "docsrs", feature(doc_cfg))]
#![allow(clippy::type_complexity)]
#![deny(missing_docs)]
#![deny(clippy::missing_const_for_fn)]
#![deny(unused_results)]

#[cfg(feature = "alloc")]
extern crate alloc;


// Documentation for concepts not specific to any one item
macro_rules! explain_type_witness {
    () => ("\
        A [type witness](crate) is \
        an enum whose variants only have [`TypeEq`] fields.
        Each variant requires the enum's type parameter to be a specific type.
    ")
}

#[macro_use]
pub mod type_fn;
mod utils;
mod type_eq;
mod type_witness_traits;

pub use crate::{
    type_eq::*,
    type_witness_traits::*,
};

#[doc(no_inline)]
pub use crate::type_fn::{CallFn, TypeFn};

#[doc(hidden)]
pub mod __ {
    pub use core::{
        mem::ManuallyDrop,
        assert, compile_error, concat, stringify,
    };
}

