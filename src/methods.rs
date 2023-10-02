//! Generic versions of
//! [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp)
//! methods,
//! which can take any permutation of them as arguments.

pub mod zipping;

#[doc(inline)]
pub use self::zipping::{in_array, zip2, zip3, zip4};