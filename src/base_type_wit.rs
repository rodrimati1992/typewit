//! abstractions over
//! [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).


#[cfg(feature = "generic_fns")]
mod meta_prim_type_wit;

#[cfg(feature = "generic_fns")]
pub use meta_prim_type_wit::MetaBaseTypeWit;


#[cfg(feature = "rust_1_61")]
pub(crate) mod some_type_arg_is_ne;

#[cfg(feature = "rust_1_61")]
pub(crate) use self::some_type_arg_is_ne::SomeTypeArgIsNe;


#[cfg(feature = "generic_fns")]
pub mod type_constructors;

#[cfg(feature = "generic_fns")]
pub mod zipping;

#[doc(inline)]
#[cfg(feature = "generic_fns")]
pub use self::zipping::{in_array, zip2, zip3, zip4};
        


mod sealed {
    #[cfg(not(feature = "generic_fns"))]
    pub trait Sealed {}
    
    #[cfg(feature = "generic_fns")]
    pub trait Sealed {}
}


macro_rules! cond_supertrait {($($supertrait:tt)*) => {
    /// Marker trait for 
    /// [`TypeCmp`](crate::TypeCmp)/[`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe).
    /// 
    /// [`TypeEq`]: crate::TypeEq
    /// [`TypeNe`]: crate::TypeNe
    /// [`TypeCmp`]: crate::TypeCmp
    pub trait BaseTypeWitness: core::fmt::Debug + Copy $($supertrait)* + sealed::Sealed {
        /// The `L` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
        type L: ?Sized;
        /// The `R` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
        type R: ?Sized;
        
        /// The [type constructor] corresponding to this type.
        ///
        /// [type constructor]: self::type_constructors::BaseTypeWitnessTc
        #[cfg(feature = "generic_fns")]
        type TypeCtor: type_constructors::BaseTypeWitnessTc<Type<Self::L, Self::R> = Self>;

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

    #[cfg(feature = "generic_fns")]
    type TypeCtor = type_constructors::TcTypeEq;

}

impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeNe<L, R> {}
impl<L: ?Sized, R: ?Sized> BaseTypeWitness for crate::TypeNe<L, R> {
    type L = L;
    type R = R;

    #[cfg(feature = "generic_fns")]
    type TypeCtor = type_constructors::TcTypeNe;
}

#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeCmp<L, R> {}
#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> BaseTypeWitness for crate::TypeCmp<L, R> {
    type L = L;
    type R = R;

    #[cfg(feature = "generic_fns")]
    type TypeCtor = type_constructors::TcTypeCmp;
}



