//! Everything related to zipping [`BaseTypeWitness`]es

use crate::{HasTypeWitness, TypeCmp, TypeEq, TypeNe};

use crate::base_type_wit::{
    type_constructors::{
        BaseTypeWitnessTc,
        TcToBaseTypeWitness,
        TcTypeCmp, TcTypeEq, TcTypeNe,
    },
    MetaBaseTypeWit,
    MetaBaseTypeWit as MBTW,
    BaseTypeWitness,
    BaseTypeWitness as BTW,
};

//////////////////////////////////////////////////////////////////////

// A TypeNe that's impossible to soundly make,
type ImpTypeNe = TypeNe<(), ()>;

// The first TypeNe in the 4 `BaseTypeWitness` type parameters
enum SomeTypeArgIsNe<A: BTW,  B: BTW, C: BTW = ImpTypeNe, D: BTW = ImpTypeNe> {
    A(TypeEq<A, TypeNe<A::L, A::R>>),
    B(TypeEq<B, TypeNe<B::L, B::R>>),
    C(TypeEq<C, TypeNe<C::L, C::R>>),
    D(TypeEq<D, TypeNe<D::L, D::R>>),
}

impl<A: BTW, B: BTW, C: BTW, D: BTW> SomeTypeArgIsNe<A, B, C, D> {
    const TRY_NEW: Option<Self> = {
        match (A::WITNESS, B::WITNESS, C::WITNESS, D::WITNESS) {
            (MBTW::Ne(ne), _, _, _) => Some(Self::A(ne)),
            (_, MBTW::Ne(ne), _, _) => Some(Self::B(ne)),
            (_, _, MBTW::Ne(ne), _) => Some(Self::C(ne)),
            (_, _, _, MBTW::Ne(ne)) => Some(Self::D(ne)),
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

impl<A: BTW, B: BTW> SomeTypeArgIsNe<A, B, ImpTypeNe, ImpTypeNe> 
where
    A::L: Sized,
    A::R: Sized,
{
    #[inline(always)]
    const fn zip2(self, _: A, _: B) -> TypeNe<(A::L, B::L), (A::R, B::R)> {
        // SAFETY: either `A` or `B` is a TypeNe (ImpTypeNe can't be constructed),
        //         therefore: `(A::L, B::L) != (A::R, B::R)`.
        unsafe { TypeNe::new_unchecked() }
    }
}
impl<A: BTW, B: BTW, C: BTW> SomeTypeArgIsNe<A, B, C, ImpTypeNe> 
where
    A::L: Sized,
    A::R: Sized,
    B::L: Sized,
    B::R: Sized,
{
    #[inline(always)]
    const fn zip3(self, _: A, _: B, _: C) -> TypeNe<(A::L, B::L, C::L), (A::R, B::R, C::R)> {
        // SAFETY: either `A`, `B`, or `C is a TypeNe (ImpTypeNe can't be constructed),
        //         therefore: `(A::L, B::L, C::L) != (A::R, B::R, C::R)`.
        unsafe { TypeNe::new_unchecked() }
    }
}
impl<A: BTW, B: BTW, C: BTW, D: BTW> SomeTypeArgIsNe<A, B, C, D> 
where
    A::L: Sized,
    A::R: Sized,
    B::L: Sized,
    B::R: Sized,
    C::L: Sized,
    C::R: Sized,
{
    #[inline(always)]
    const fn zip4(
        self,
        _: A,
        _: B,
        _: C,
        _: D,
    ) -> TypeNe<(A::L, B::L, C::L, D::L), (A::R, B::R, C::R, D::R)> {
        // SAFETY: either `A`, `B`, `C`, or `D` is a TypeNe,
        //         therefore: `(A::L, B::L, C::L, D::L) != (A::R, B::R, C::R, D::R)`.
        unsafe { TypeNe::new_unchecked() }
    }
}

//////////////////////////////////////////////////////////////////////


#[doc(hidden)]
pub trait ZipTc: 'static + Copy {
    type Output: BaseTypeWitnessTc;
}

#[doc(hidden)]
pub type ZipTcOut<TupleOfTc> = <TupleOfTc as ZipTc>::Output;


impl ZipTc for (TcTypeEq, TcTypeEq) {
    type Output = TcTypeEq;
}
impl ZipTc for (TcTypeEq, TcTypeNe) {
    type Output = TcTypeNe;
}
impl ZipTc for (TcTypeEq, TcTypeCmp) {
    type Output = TcTypeCmp;
}

impl<B: BaseTypeWitnessTc> ZipTc for (TcTypeNe, B) {
    type Output = TcTypeNe;
}

impl ZipTc for (TcTypeCmp, TcTypeEq) {
    type Output = TcTypeCmp;
}

impl ZipTc for (TcTypeCmp, TcTypeNe) {
    type Output = TcTypeNe;
}

impl ZipTc for (TcTypeCmp, TcTypeCmp) {
    type Output = TcTypeCmp;
}

impl<A, B, C> ZipTc for (A, B, C)
where
    A: BaseTypeWitnessTc,
    B: BaseTypeWitnessTc,
    C: BaseTypeWitnessTc,
    (A, B): ZipTc,
    (ZipTcOut<(A, B)>, C): ZipTc,
{
    type Output = ZipTcOut<(ZipTcOut<(A, B)>, C)>;
}

impl<A, B, C, D> ZipTc<> for (A, B, C, D)
where
    A: BaseTypeWitnessTc,
    B: BaseTypeWitnessTc,
    C: BaseTypeWitnessTc,
    D: BaseTypeWitnessTc,
    (A, B): ZipTc,
    (C, D): ZipTc,
    (ZipTcOut<(A, B)>, ZipTcOut<(C, D)>): ZipTc,
{
    type Output = ZipTcOut<(ZipTcOut<(A, B)>, ZipTcOut<(C, D)>)>;
}


//////////////////////////////////////////////////////////////////////

macro_rules! declare_zip_items {
    (
        [$first_typa:ident ($($middle_typa:ident)*) $end_typa:ident] 

        $(#[$fn_attr:meta])*
        fn $fn_name:ident($fn_param0:ident $(, $fn_param_rem:ident)*);

        $($rem:tt)*
    ) => {
        __declare_zip_items!{
            [$first_typa ($($middle_typa)*) $end_typa] 
            [$first_typa $($middle_typa)* $end_typa] 

            $(#[$fn_attr])*
            fn $fn_name ($fn_param0 $(, $fn_param_rem)*) ($fn_param0, $(, $fn_param_rem)*);

            $($rem)*
        }
    }
}
macro_rules! __declare_zip_items {
    (
        [$first_typa:ident ($($middle_typa:ident)*) $end_typa:ident] 
        [$($ty_params:ident)*]

        $(#[$fn_attr:meta])*
        fn $fn_name:ident($($fn_param:ident),*) ($fn_param0:ident, $(, $fn_param_rem:ident)*);

        $(#[$type_alias_attr:meta])*
        type $type_alias:ident;

        $(#[$trait_attr:meta])*
        trait $trait:ident;

        enum $ty_wit:ident ($($arg_wit:ident),*);


        zip_method = $zip_method:ident;
        count = $count:tt
    ) => {
        #[doc = concat!(
            "The type returned by zipping ",
            $count,
            " [`BaseTypeWitness`]es.",
        )]
        $(#[$type_alias_attr])*
        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
        pub type $type_alias<$($ty_params),*> = 
            <$first_typa as $trait<$($middle_typa,)* $end_typa>>::Output;

        #[doc = concat!(
            "Computes the type that the [`",
            stringify!($fn_name),
            "`]  function returns, ",
        )]
        $(#[$trait_attr])*
        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
        pub trait $trait<
            $($middle_typa: BaseTypeWitness,)*
            $end_typa: BaseTypeWitness,
        >: BaseTypeWitness 
        where
            Self::L: Sized,
            Self::R: Sized,
            $(
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
        {
            #[doc = concat!(
                "The the type returned by zipping ",
                $count, " [`BaseTypeWitness`] types"
            )]
            type Output: BaseTypeWitness<
                L = (Self::L, $($middle_typa::L,)* $end_typa::L),
                R = (Self::R, $($middle_typa::R,)* $end_typa::R),
            >;
        }

        impl<$($ty_params,)*> $trait<$($middle_typa,)* $end_typa> for $first_typa
        where
            $($ty_params: BaseTypeWitness,)*
            ($($ty_params::TypeCtor,)*): ZipTc,

            Self::L: Sized,
            Self::R: Sized,
            $(
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
            
        {
            type Output = 
                TcToBaseTypeWitness<
                    ZipTcOut<($($ty_params::TypeCtor,)*)>,
                    ($($ty_params::L,)*),
                    ($($ty_params::R,)*),
                >;
        }


        enum $ty_wit<$($ty_params,)* Ret: BaseTypeWitness> 
        where
            $( $ty_params: BaseTypeWitness, )*
            $first_typa::L: Sized,
            $first_typa::R: Sized,
            $(
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
        {
            Eq {
                $($arg_wit: TypeEq<$ty_params, TypeEq<$ty_params::L, $ty_params::R>>,)*
                ret: TypeEq<
                    Ret, 
                    TypeEq<($($ty_params::L,)*), ($($ty_params::R,)*)>
                >
            },
            Ne {
                #[allow(dead_code)]
                contains_ne: SomeTypeArgIsNe<$($ty_params,)*>,
                ret: TypeEq<Ret, TypeNe<($($ty_params::L,)*), ($($ty_params::R,)*)>>
            },
            Cmp {
                ret: TypeEq<Ret, TypeCmp<($($ty_params::L,)*), ($($ty_params::R,)*)>>
            },
        }

        impl<$($ty_params,)*> $ty_wit<$($ty_params,)* $type_alias<$($ty_params,)*>> 
        where
            $first_typa: $trait<$($middle_typa,)* $end_typa>,
            $first_typa::L: Sized,
            $first_typa::R: Sized,
            $(
                $middle_typa: BaseTypeWitness,
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
            $end_typa: BaseTypeWitness,
        {
            const NEW: Self = 
                match ($($ty_params::WITNESS,)* $type_alias::<$($ty_params,)*>::WITNESS) {
                    ($(MBTW::Eq($arg_wit),)* MBTW::Eq(ret)) => {
                        Self::Eq { $($arg_wit,)* ret }
                    }
                    (.., MBTW::Ne(ret)) => {
                        let contains_ne = SomeTypeArgIsNe::<$($ty_params,)*>::new();
                        Self::Ne { contains_ne, ret }
                    }
                    (.., MBTW::Cmp(ret)) => {
                        Self::Cmp {ret}
                    }
                    _ => panic!("BUG: invalid permutation of $trait"),
                };
        }


        $(#[$fn_attr])*
        #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
        pub const fn $fn_name<$($ty_params,)*>(
            $($fn_param: $ty_params,)*
        ) -> $type_alias<$($ty_params,)*>
        where
            $first_typa: BaseTypeWitness,
            $first_typa::L: Sized,
            $first_typa::R: Sized,
            $(
                $middle_typa: BaseTypeWitness,
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
            $end_typa: BaseTypeWitness,
            
            $first_typa: $trait<$($middle_typa,)* $end_typa>
        {
            match $ty_wit::<$($ty_params,)* $type_alias<$($ty_params,)*>>::NEW {
                $ty_wit::Eq {$($arg_wit,)* ret} => 
                    ret.to_left(TypeEq::$zip_method($($arg_wit.to_right($fn_param),)*)),
                $ty_wit::Ne {contains_ne, ret} => 
                    ret.to_left(contains_ne.$fn_name($($fn_param,)*)),
                $ty_wit::Cmp {ret} => 
                    ret.to_left(
                        MetaBaseTypeWit::to_cmp(A::WITNESS, $fn_param0)
                            .$zip_method($($fn_param_rem,)*)
                    ),
            }
        }
    };
}



declare_zip_items!{
    [A () B] 

    /// Zips together two [`BaseTypeWitness`] types.
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
    /// use typewit::base_type_wit::zip2;
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
    fn zip2(wit0, wit1);

    type Zip2Out;

    trait Zip2;

    enum Zip2Wit (arg0, arg1);

    zip_method = zip;

    count = "two"
}

//////////////////////////////////////////////////////////////////////

declare_zip_items!{
    [A (B) C] 

    /// Zips together three [`BaseTypeWitness`] types.
    ///
    /// This returns the most specific of the
    /// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp)
    /// types, depending on the argument types.
    ///
    /// # Example
    ///
    /// ### Basic
    ///
    /// This example shows basic usage.
    /// 
    /// ```rust
    /// use typewit::{TypeCmp, TypeEq, TypeNe, type_eq};
    /// use typewit::base_type_wit::zip3;
    ///
    /// with::<u8, u8, bool, u16, u32>(TypeEq::NEW, TypeNe::with_any().unwrap(), TypeCmp::with_any());
    ///
    /// const fn with<A, B, C, D, E>(eq: TypeEq<A, B>, ne: TypeNe<B, C>, cmp: TypeCmp<D, E>) {
    ///     let _: TypeEq<(A, B, i64), (B, A, i64)> = zip3(eq, eq.flip(), type_eq::<i64>());
    ///     let _: TypeNe<(A, B, B), (B, A, C)> = zip3(eq, eq.flip(), ne);
    ///     let _: TypeCmp<(A, D, B), (B, E, A)> = zip3(eq, cmp, eq.flip());
    /// }
    /// ```
    fn zip3(wit0, wit1, wit2);

    type Zip3Out;

    trait Zip3;

    enum Zip3Wit (arg0, arg1, arg2);

    zip_method = zip3;

    count = "three"
}


//////////////////////////////////////////////////////////////////////



declare_zip_items!{
    [A (B C) D] 

    /// Zips together four [`BaseTypeWitness`] types.
    ///
    /// This returns the most specific of the
    /// [`TypeEq`](crate::TypeEq)/[`TypeNe`](crate::TypeNe)/[`TypeCmp`](crate::TypeCmp)
    /// types, depending on the argument types.
    ///
    /// # Example
    ///
    /// ### Basic
    ///
    /// This example shows basic usage.
    /// 
    /// ```rust
    /// use typewit::{TypeCmp, TypeEq, TypeNe, type_eq};
    /// use typewit::base_type_wit::zip4;
    ///
    /// with::<u8, u8, bool, u16, u32>(TypeEq::NEW, TypeNe::with_any().unwrap(), TypeCmp::with_any());
    ///
    /// const fn with<A, B, C, D, E>(eq: TypeEq<A, B>, ne: TypeNe<B, C>, cmp: TypeCmp<D, E>) {
    ///     let _: TypeEq<(A, u64, B, i64), (B, u64, A, i64)> = 
    ///         zip4(eq, type_eq(), eq.flip(), type_eq());
    ///     let _: TypeNe<(A, E, B, B), (B, D, A, C)> = zip4(eq, cmp.flip(), eq.flip(), ne);
    ///     let _: TypeCmp<(D, A, B, A), (E, B, A, B)> = zip4(cmp, eq, eq.flip(), eq);
    /// }
    /// ```
    fn zip4(wit0, wit1, wit2, wit3);

    type Zip4Out;

    trait Zip4;

    enum Zip4Wit (arg0, arg1, arg2, arg3);

    zip_method = zip4;

    count = "four"
}
