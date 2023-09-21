mod sealed {
    pub trait Sealed {}
}


/// Marker trait for 
/// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp).
/// 
/// [`TypeEq`]: crate::TypeEq
/// [`TypeNe`]: crate::TypeNe
/// [`TypeCmp`]: crate::TypeCmp
pub trait PrimTypeWitness: core::fmt::Debug + Copy + sealed::Sealed {
    /// The `L` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
    type L: ?Sized;
    /// The `R` type parameter of `TypeEq`/`TypeNe`/`TypeCmp` types.
    type R: ?Sized;
}

impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeEq<L, R> {}
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeEq<L, R> {
    type L = L;
    type R = R;
}

impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeNe<L, R> {}
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeNe<L, R> {
    type L = L;
    type R = R;
}

#[cfg(feature = "cmp")]
impl<L: ?Sized, R: ?Sized> sealed::Sealed for crate::TypeCmp<L, R> {}
impl<L: ?Sized, R: ?Sized> PrimTypeWitness for crate::TypeCmp<L, R> {
    type L = L;
    type R = R;
}



