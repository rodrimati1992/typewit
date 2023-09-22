//! abstractions over
//! [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).


#[cfg(feature = "generic_fns")]
mod meta_prim_type_wit;

#[cfg(feature = "generic_fns")]
pub use meta_prim_type_wit::MetaPrimTypeWit;


#[cfg(feature = "generic_fns")]
mod zipping;

#[cfg(feature = "generic_fns")]
pub use zipping::{
    Zip2, Zip2Out, zip2,
};

mod sealed {
    #[cfg(not(feature = "generic_fns"))]
    pub trait Sealed<L: ?Sized, R: ?Sized> {}
    
    #[cfg(feature = "generic_fns")]
    pub trait Sealed<L: ?Sized, R: ?Sized>: 
        Sized + crate::HasTypeWitness<super::MetaPrimTypeWit<L, R, Self>> 
    {}
}


/// Marker trait for 
/// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).
/// 
/// [`TypeEq`]: crate::TypeEq
/// [`TypeNe`]: crate::TypeNe
/// [`TypeCmp`]: crate::TypeCmp
pub trait PrimTypeWitness: core::fmt::Debug + Copy + sealed::Sealed<Self::L, Self::R> {
    /// The `L` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
    type L: ?Sized;
    /// The `R` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
    type R: ?Sized;
}

impl<L: ?Sized, R: ?Sized> sealed::Sealed<L, R> for crate::TypeEq<L, R> {}
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeEq<L, R> {
    type L = L;
    type R = R;
}

impl<L: ?Sized, R: ?Sized> sealed::Sealed<L, R> for crate::TypeNe<L, R> {}
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeNe<L, R> {
    type L = L;
    type R = R;
}

#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> sealed::Sealed<L, R> for crate::TypeCmp<L, R> {}
#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeCmp<L, R> {
    type L = L;
    type R = R;
}



