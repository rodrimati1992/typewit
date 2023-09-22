use crate::{HasTypeWitness, BaseTypeWitness, TypeEq, TypeNe, TypeCmp};

use crate::base_type_wit::{MetaBaseTypeWit as MBTW, BaseTypeWitness as PTW};

//////////////////////////////////////////////////////////////////////

// The first TypeNe in the 4 `BaseTypeWitness` type parameters
enum SomeTypeArgIsNe<A: PTW,  B: PTW, C: PTW = B, D: PTW = B> {
    A(TypeEq<A, TypeNe<A::L, A::R>>),
    B(TypeEq<B, TypeNe<B::L, B::R>>),
    C(TypeEq<C, TypeNe<C::L, C::R>>),
    D(TypeEq<D, TypeNe<D::L, D::R>>),
}

impl<A: PTW, B: PTW, C: PTW, D: PTW> SomeTypeArgIsNe<A, B, C, D> {
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
    ) => {
        $(#[$type_alias_attr])*
        pub type $type_alias<$($ty_params),*> =
            <$first_typa as $trait<$($middle_typa,)* $end_typa>>::Output;

        $(#[$trait_attr])*
        pub trait $trait<$($middle_typa,)* $end_typa>: BaseTypeWitness 
        where
            Self::L: Sized,
            Self::R: Sized,
            $(
                $middle_typa: BaseTypeWitness,
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
            $end_typa: BaseTypeWitness,
        {
            #[doc = concat!(
                "The type returned by zipping `Self` with `",
                $( stringify!($middle_typa), "` and `", )*
                stringify!($end_typa), "`"
            )]
            type Output: BaseTypeWitness<
                L = (Self::L, $($middle_typa::L, )* $end_typa::L),
                R = (Self::R, $($middle_typa::R, )* $end_typa::R)
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
        pub const fn $fn_name<$($ty_params,)*>(
            $($fn_param: $ty_params,)*
        ) -> $type_alias<$($ty_params,)*>
        where
            $first_typa: BaseTypeWitness + $trait<$($middle_typa,)* $end_typa>,
            $first_typa::L: Sized,
            $first_typa::R: Sized,
            $(
                $middle_typa: BaseTypeWitness,
                $middle_typa::L: Sized,
                $middle_typa::R: Sized,
            )*
            $end_typa: BaseTypeWitness,
        {
            match $ty_wit::<$($ty_params,)* $type_alias<$($ty_params,)*>>::NEW {
                $ty_wit::Eq {$($arg_wit,)* ret} => 
                    ret.to_left(
                        TypeEq::$zip_method(
                            $($arg_wit.to_right($fn_param),)*
                        )
                    ),
                $ty_wit::Ne {contains_ne: _, ret} => 
                    // SAFETY: `contains_ne: SomeTypeArgIsNe<A, B, .., D>` proves that 
                    //         one of `A`..=`D` is a TypeNe,
                    //         therefore: `(A::L, B::L, .., D::L) != (A::R, B::R, .., D::R)`.
                    unsafe { ret.to_left(TypeNe::new_unchecked()) },
                $ty_wit::Cmp {ret} => {
                    ret.to_left(
                        MBTW::to_cmp(A::WITNESS, $fn_param0)
                            .$zip_method($($fn_param_rem,)*)
                    )
                }
            }
        }




    };
}



declare_zip_items!{
    [A () B] 

    /// Zips toghether two [`BaseTypeWitness`] types.
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
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    fn zip2(wit0, wit1);

    /// The type returned by zipping `A:`[`BaseTypeWitness`] with `B:`[`BaseTypeWitness`]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    type Zip2Out;

    /// Queries the type returned by zipping `Self` with `B`
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    trait Zip2;

    enum Zip2Wit (arg0, arg1);

    zip_method = zip;
}

macro_rules! impl_zip2 {
    ($lhs:ident , $rhs:ident => $out:ident) => {
        impl<AL, AR, BL: ?Sized, BR: ?Sized> Zip2<$rhs<BL, BR>> for $lhs<AL, AR> {
            type Output = $out<(AL, BL), (AR, BR)>;
        }
    };
}


impl_zip2!{
    TypeEq, TypeEq => TypeEq
}
impl_zip2!{
    TypeEq, TypeNe => TypeNe
}
impl_zip2!{
    TypeEq, TypeCmp => TypeCmp
}

impl<AL, AR, B: BaseTypeWitness> Zip2<B> for TypeNe<AL, AR> {
    type Output = TypeNe<(AL, B::L), (AR, B::R)>;
}

impl_zip2!{
    TypeCmp, TypeEq => TypeCmp
}

impl_zip2!{
    TypeCmp, TypeNe => TypeNe
}

impl_zip2!{
    TypeCmp, TypeCmp => TypeCmp
}

//////////////////////////////////////////////////////////////////////

declare_zip_items!{
    [A (B) C] 

    /// Zips toghether three [`BaseTypeWitness`] types.
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
    ///     let _: TypeCmp<(A, B, D), (B, A, E)> = zip3(eq, eq.flip(), cmp);
    /// 
    ///     let _: TypeNe<(B, A, A), (C, B, B)> = zip3(ne, eq, eq);
    ///     let _: TypeNe<(B, A, C), (C, B, B)> = zip3(ne, eq, ne.flip());
    ///     let _: TypeNe<(B, A, D), (C, B, E)> = zip3(ne, eq, cmp);
    /// 
    ///     let _: TypeCmp<(D, A, A), (E, B, B)> = zip3(cmp, eq, eq);
    ///     let _: TypeNe<(D, A, B), (E, B, C)> = zip3(cmp, eq, ne);
    ///     let _: TypeCmp<(D, A, E), (E, B, D)> = zip3(cmp, eq, cmp.flip());
    /// 
    /// }
    /// ```
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    fn zip3(wit0, wit1, wit2);

    /// The type returned by zipping `A:`[`BaseTypeWitness`] with 
    /// `B:`[`BaseTypeWitness`] and `C:`[`BaseTypeWitness`]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    type Zip3Out;

    /// Queries the type returned by zipping `Self` with `B` and `C`
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
    trait Zip3;

    enum Zip3Wit (arg0, arg1, arg2);

    zip_method = zip3;
}

impl<A, B, C> Zip3<B, C> for A
where
    A: BaseTypeWitness,
    B: BaseTypeWitness,
    C: BaseTypeWitness,
    A::L: Sized,
    A::R: Sized,
    B::L: Sized,
    B::R: Sized,
    A: Zip2<B>,
    Zip2Out<A, B>: Zip2<C>,
    Zip2Out<Zip2Out<A, B>, C>: Zip3Flattener,
    <Zip2Out<Zip2Out<A, B>, C> as Zip3Flattener>::Flattened: BaseTypeWitness<
        L = (A::L, B::L, C::L),
        R = (A::R, B::R, C::R),
    >
{
    type Output = <Zip2Out<Zip2Out<A, B>, C> as Zip3Flattener>::Flattened;
}

/// Helper trait for Zip3
pub trait Zip3Flattener: BaseTypeWitness {
    type Flattened: BaseTypeWitness;
}

macro_rules! impl_zip3helper {
    ($($witness:ident)*) => {
        $(
            impl<LA, LB, LC: ?Sized, RA, RB, RC: ?Sized>
                Zip3Flattener 
            for $witness<((LA, LB), LC), ((RA, RB), RC)> 
            {
                type Flattened = $witness<(LA, LB, LC), (RA, RB, RC)>;
            }
        )*
    }
}
impl_zip3helper!{TypeCmp TypeEq TypeNe}
