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
//! use typewit::{MakeTypeWitness, TypeEq};
//! 
//! assert_eq!(returnal::<u8>(), 3);
//! assert_eq!(returnal::<&str>(), "hello");
//! 
//! 
//! const fn returnal<'a, R>() -> R
//! where
//!     RetWitness<'a, R>: MakeTypeWitness,
//! {
//!     match MakeTypeWitness::MAKE {
//!         RetWitness::U8(te) => {
//!             // `te` (a `TypeEq<R, u8>`) allows coercing between `R` and `u8`,
//!             // because `TypeEq` is a value-level proof that both types are the same.
//!             // `te.to_left(...)` goes from `u8` to `R`.
//!             te.to_left(3u8)
//!         }
//!         RetWitness::Str(te) => {
//!             // `te` is a `TypeEq<R, &'a str>`
//!             // `te.to_left(...)` goes from `&'a str` to `R`.
//!             te.to_left("hello")
//!         }
//!     }
//! }
//! 
//! // This macro declares a type witness enum
//! typewit::simple_type_witness! {
//!     // Declares `enum RetWitness<'a, __Wit>` 
//!     // (the `__Wit` type parameter is implicitly added after all generics)
//!     enum RetWitness['a] {
//!         // This variant requires `__Wit == u8`
//!         U8 = u8,
//!    
//!         // This variant requires `__Wit == &'a str`
//!         Str = &'a str,
//!     }
//! }
//! ```
//! 
//! <span id="example1"></span>
//! ### Indexing polymorphism
//! 
//! This function demonstrates const fn polymorphism
//! and projecting [`TypeEq`] by implementing [`TypeFn`].
//! 
//! (this example requires Rust 1.61.0, because of the `I: SliceIndex<T>,` bound)
#![cfg_attr(not(feature = "rust_1_61"), doc = "```ignore")]
#![cfg_attr(feature = "rust_1_61", doc = "```rust")]
//! use std::ops::Range;
//! 
//! use typewit::{HasTypeWitness, TypeEq};
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
//! const fn index<T, I>(slice: &[T], idx: I) -> &SliceIndexRet<I, T>
//! where
//!     I: SliceIndex<T>,
//! {
//!     // `I::WITNESS` is `<I as HasTypeWitness<IndexWitness<I>>>::WITNESS`,
//!     match I::WITNESS {
//!         IndexWitness::Usize(arg_te) => {
//!             // `arg_te` (a `TypeEq<I, usize>`) allows coercing between `I` and `usize`,
//!             // because `TypeEq` is a value-level proof that both types are the same.
//!             let idx: usize = arg_te.to_right(idx);
//! 
//!             // using the `TypeFn` impl for `FnSliceIndexRet<T>` to 
//!             // map `TypeEq<I, usize>` 
//!             // to  `TypeEq<SliceIndexRet<I, T>, SliceIndexRet<usize, T>>`
//!             arg_te.project::<FnSliceIndexRet<T>>()
//!                 // converts`TypeEq<SliceIndexRet<I, T>, T>` 
//!                 //      to `TypeEq<&SliceIndexRet<I, T>, &T>`
//!                 .in_ref()
//!                 .to_left(&slice[idx])
//!         }
//!         IndexWitness::Range(arg_te) => {
//!             let range: Range<usize> = arg_te.to_right(idx);
//!             let ret: &[T] = slice_range(slice, range);
//!             arg_te.project::<FnSliceIndexRet<T>>().in_ref().to_left(ret)
//!         }
//!     }
//! }
//! 
//! // This macro declares a type witness enum
//! typewit::simple_type_witness! {
//!     // Declares `enum IndexWitness<__Wit>` 
//!     // (the `__Wit` type parameter is implicitly added after all generics)
//!     enum IndexWitness {
//!         // This variant requires `__Wit == usize`
//!         Usize = usize,
//!    
//!         // This variant requires `__Wit == Range<usize>`
//!         Range = Range<usize>,
//!     }
//! }
//! 
//! /// Trait for all types that can be used as slice indices
//! /// 
//! /// The `HasTypeWitness` supertrait allows getting a `IndexWitness<Self>`
//! /// with its `WITNESS` associated constant.
//! trait SliceIndex<T>: HasTypeWitness<IndexWitness<Self>> + Sized {
//!     type Returns: ?Sized;
//! }
//! impl<T> SliceIndex<T> for usize {
//!     type Returns = T;
//! }
//! impl<T> SliceIndex<T> for Range<usize> {
//!     type Returns = [T];
//! }
//! 
//! type SliceIndexRet<I, T> = <I as SliceIndex<T>>::Returns;
//! 
//! // This is a type-level function from `I` to `<I as SliceIndex<T>>::Returns`
//! struct FnSliceIndexRet<T>(std::marker::PhantomData<fn() -> T>);
//! 
//! // What makes FnSliceIndexRet a type-level function
//! impl<T, I: SliceIndex<T>> typewit::TypeFn<I> for FnSliceIndexRet<T>  {
//!     type Output = SliceIndexRet<I, T>;
//! }
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
//! - `"rust_1_61"`: allows the `typewit` crate to use Rust 1.61.0 features.
//! 
//! - `"rust_stable"`: enables all the `"rust_1_*"` features.
//! 
//! - `"alloc"`: enable items that use anything from the standard `alloc` crate.
//! 
//! - `"mut_refs"`: turns functions that take mutable references into const fns.
//! note: as of April 2023, 
//! this crate feature requires a stable compiler from the future.
//! 
//! - `"nightly_mut_refs"`(requires the nightly compiler):
//! Enables the `"mut_refs"` crate feature and 
//! the `const_mut_refs` nightly feature.
//! 
//! None of the crate features are enabled by default.
//! 
//! # No-std support
//! 
//! `typewit` is `#![no_std]`, it can be used anywhere Rust can be used.
//! You need to enable the `"alloc"` feature to enable items that use anything 
//! from the standard `alloc` crate.
//! 
//! # Minimum Supported Rust Version
//! 
//! `typewit` supports Rust 1.57.0.
//! 
//! Features that require newer versions of Rust, or the nightly compiler,
//! need to be explicitly enabled with crate features.
//! 
//! 
//! 
//! [`TypeEq`]: crate::TypeEq
//! [`TypeFn`]: crate::type_fn::TypeFn
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
        an enum whose variants only have [`TypeEq`](crate::TypeEq) fields.
        Each variant requires the enum's type parameter to be a specific type.
    ")
}

#[macro_use]
pub mod type_fn;
mod utils;
mod macros;
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
        clone::Clone,
        cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering},
        fmt::{Debug, Formatter, Result as FmtResult},
        hash::{Hash, Hasher},
        marker::Copy,
        mem::{ManuallyDrop, discriminant},
        option::Option,
        primitive::{bool, usize},
        assert, compile_error, concat, stringify,
    };

    pub use crate::utils::TypeIdentity;
}
