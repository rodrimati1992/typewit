use crate::{HasTypeWitness, PrimTypeWitness, TypeEq, TypeNe, TypeCmp};

use crate::prim_type_wit::{MetaPrimTypeWit as MPTW, PrimTypeWitness as PTW};


//////////////////////////////////////////////////////////////////////

// The first TypeNe in the 4 `PrimTypeWitness` type parameters
enum SomeTypeArgIsNe<A: PTW,  B: PTW, C: PTW = B, D: PTW = B> {
    A(TypeEq<A, TypeNe<A::L, A::R>>),
    B(TypeEq<B, TypeNe<B::L, B::R>>),
    C(TypeEq<C, TypeNe<C::L, C::R>>),
    D(TypeEq<D, TypeNe<D::L, D::R>>),
}

impl<A: PTW, B: PTW, C: PTW, D: PTW> SomeTypeArgIsNe<A, B, C, D> {
    const TRY_NEW: Option<Self> = {
        match (A::WITNESS, B::WITNESS, C::WITNESS, D::WITNESS) {
            (MPTW::Ne(ne), _, _, _) => Some(Self::A(ne)),
            (_, MPTW::Ne(ne), _, _) => Some(Self::B(ne)),
            (_, _, MPTW::Ne(ne), _) => Some(Self::C(ne)),
            (_, _, _, MPTW::Ne(ne)) => Some(Self::D(ne)),
            _ => None,
        }
    };

    const fn new() -> Self {
        match Self::TRY_NEW {
            Some(x) => x,
            None => panic!("expected at least one type argument to be TypeNe"),
        }
    }
}

//////////////////////////////////////////////////////////////////////

/// The type returned by zipping `A:`[`PrimTypeWitness`] with `B:`[`PrimTypeWitness`]
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
pub type Zip2Out<A, B> = <A as Zip2<B>>::Output;

/// Queries the type returned by zipping `Self` with `Rhs`
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
pub trait Zip2<Rhs>: PrimTypeWitness 
where
    Self::L: Sized,
    Self::R: Sized,
    Rhs: PrimTypeWitness,
{
    /// The type returned by zipping `Self` with `Rhs`
    type Output: PrimTypeWitness<L = (Self::L, Rhs::L), R = (Self::R, Rhs::R)>;
}

macro_rules! impl_zip_with {
    ($lhs:ident , $rhs:ident => $out:ident) => {
        impl<AL, AR, BL: ?Sized, BR: ?Sized> Zip2<$rhs<BL, BR>> for $lhs<AL, AR> {
            type Output = $out<(AL, BL), (AR, BR)>;
        }
    };
}


impl_zip_with!{
    TypeEq, TypeEq => TypeEq
}
impl_zip_with!{
    TypeEq, TypeNe => TypeNe
}
impl_zip_with!{
    TypeEq, TypeCmp => TypeCmp
}

impl<AL, AR, B: PrimTypeWitness> Zip2<B> for TypeNe<AL, AR> {
    type Output = TypeNe<(AL, B::L), (AR, B::R)>;
}

impl_zip_with!{
    TypeCmp, TypeEq => TypeCmp
}

impl_zip_with!{
    TypeCmp, TypeNe => TypeNe
}

impl_zip_with!{
    TypeCmp, TypeCmp => TypeCmp
}

////////////////////////////////////////////////////////////////////////

enum Zip2Wit<A, B, Ret: PrimTypeWitness> 
where
    A: PrimTypeWitness, 
    B: PrimTypeWitness,
    A::L: Sized,
    A::R: Sized,
{
    Eq {
        arg0: TypeEq<A, TypeEq<A::L, A::R>>,
        arg1: TypeEq<B, TypeEq<B::L, B::R>>,
        ret: TypeEq<Ret, TypeEq<(A::L, B::L), (A::R, B::R)>>
    },
    Ne {
        #[allow(dead_code)]
        contains_ne: SomeTypeArgIsNe<A, B>,
        ret: TypeEq<Ret, TypeNe<(A::L, B::L), (A::R, B::R)>>
    },
    Cmp {
        ret: TypeEq<Ret, TypeCmp<(A::L, B::L), (A::R, B::R)>>
    },
}

impl<A, B> Zip2Wit<A, B, Zip2Out<A, B>> 
where
    A: Zip2<B>,
    B: PrimTypeWitness,
    A::L: Sized,
    A::R: Sized,
{
    const ZIP2_WIT: Self = match (A::WITNESS, B::WITNESS, Zip2Out::<A, B>::WITNESS) {
        (MPTW::Eq(arg0), MPTW::Eq(arg1), MPTW::Eq(ret)) => {
            Self::Eq { arg0, arg1, ret }
        }
        (_, _, MPTW::Ne(ret)) => {
            let contains_ne = SomeTypeArgIsNe::<A, B>::new();
            Self::Ne { contains_ne, ret }
        }
        (_, _, MPTW::Cmp(ret)) => {
            Self::Cmp {ret}
        }
        _ => panic!("BUG: invalid permutation of Zip2"),
    };
}


/// Zips toghether two [`PrimTypeWitness`] types.
///
/// This returns the most specific of the
/// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp)
/// types, depending on the argument types.
///
/// # Example
///
/// ### Basic
///
/// This example shows all permutations of argument and return types.
///
/// ```rust
/// use typewit::{TypeCmp, TypeEq, TypeNe};
/// use typewit::prim_type_wit::zip2;
///
/// with::<u8, u8, bool, u16, u32>(TypeEq::NEW, TypeNe::with_any().unwrap(), TypeCmp::with_any());
///
/// const fn with<A, B, C, D, E>(eq: TypeEq<A, B>, ne: TypeNe<B, C>, cmp: TypeCmp<D, E>) {
///     let _: TypeEq<(A, B), (B, A)> = zip2(eq, eq.flip());
///     let _: TypeNe<(A, B), (B, C)> = zip2(eq, ne);
///     let _: TypeCmp<(A, D), (B, E)> = zip2(eq, cmp);
/// 
///     let _: TypeNe<(B, A), (C, B)> = zip2(ne, eq);
///     let _: TypeNe<(B, C), (C, B)> = zip2(ne, ne.flip());
///     let _: TypeNe<(B, D), (C, E)> = zip2(ne, cmp);
/// 
///     let _: TypeCmp<(D, A), (E, B)> = zip2(cmp, eq);
///     let _: TypeNe<(D, B), (E, C)> = zip2(cmp, ne);
///     let _: TypeCmp<(D, E), (E, D)> = zip2(cmp, cmp.flip());
/// 
/// }
/// ```
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
pub const fn zip2<A, B>(wit0: A, wit1: B) -> Zip2Out<A, B>
where
    A: PrimTypeWitness,
    A::L: Sized,
    A::R: Sized,
    B: PrimTypeWitness,
    A: Zip2<B>,
{
    match Zip2Wit::<A, B, Zip2Out<A, B>>::ZIP2_WIT {
        Zip2Wit::Eq {arg0, arg1, ret} => 
            ret.to_left(arg0.to_right(wit0).zip(arg1.to_right(wit1))),
        Zip2Wit::Ne {contains_ne: _, ret} => 
            // SAFETY: `contains_ne: SomeTypeArgIsNe<A, B>` proves that 
            //         `A` and/or `B` is a TypeNe,
            //         therefore: `(A::L, B::L) != (A::R, B::R)`.
            unsafe { ret.to_left(TypeNe::new_unchecked()) },
        Zip2Wit::Cmp {ret} => ret.to_left(MPTW::to_cmp(A::WITNESS, wit0).zip(wit1)),
    }
}



