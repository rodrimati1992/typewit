//! abstractions over
//! [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).


#[cfg(feature = "generic_fns")]
mod meta_prim_type_wit;

#[cfg(feature = "generic_fns")]
pub use meta_prim_type_wit::MetaBaseTypeWit;


#[cfg(feature = "generic_fns")]
mod zipping;

#[cfg(feature = "generic_fns")]
pub use zipping::{
    Zip2, Zip2Out, zip2,
    Zip3, Zip3Out, zip3,
    Zip4, Zip4Out, zip4,
};

mod sealed {
    #[cfg(not(feature = "generic_fns"))]
    pub trait Sealed {}
    
    #[cfg(feature = "generic_fns")]
    pub trait Sealed {}
}


macro_rules! cond_supertrait {($($supertrait:tt)*) => {
    /// Marker trait for 
    /// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).
    /// 
    /// [`TypeEq`]: crate::TypeEq
    /// [`TypeNe`]: crate::TypeNe
    /// [`TypeCmp`]: crate::TypeCmp
    pub trait BaseTypeWitness: core::fmt::Debug + Copy $($supertrait)* + sealed::Sealed {
        /// The `L` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
        type L: ?Sized;
        /// The `R` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
        type R: ?Sized;
    }
}}

#[cfg(not(feature = "generic_fns"))]
cond_supertrait!{}

#[cfg(feature = "generic_fns")]
cond_supertrait!{ + crate::HasTypeWitness<MetaBaseTypeWit<Self::L, Self::R, Self>> }




impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeEq<L, R> {}
impl<L: ?Sized, R: ?Sized> BaseTypeWitness for crate::TypeEq<L, R> {
    type L = L;
    type R = R;
}

impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeNe<L, R> {}
impl<L: ?Sized, R: ?Sized> BaseTypeWitness for crate::TypeNe<L, R> {
    type L = L;
    type R = R;
}

#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeCmp<L, R> {}
#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> BaseTypeWitness for crate::TypeCmp<L, R> {
    type L = L;
    type R = R;
}



