use crate::{
    type_fn::{InjTypeFn, InvokeAlias, CallInjFn, UncallFn},
    TypeEq,
    TypeNe,
};

crate::type_eq_ne_guts::declare_type_cmp_helpers!{
    $
    TypeNe
    InjTypeFn
    CallInjFn
}


#[cfg_attr(feature = "docsrs", doc(cfg(feature = "inj_type_fn")))]
impl<L: ?Sized, R: ?Sized> TypeNe<L, R> {
    /// Swaps the type arguments of this `TypeNe`
    pub const fn flip(self: TypeNe<L, R>) -> TypeNe<R, L> {
        // SAFETY: type inequality is commutative
        unsafe { TypeNe::<R, L>::new_unchecked() }
    }

    /// Joins a proof of `L != R` with a proof of `J == L`,
    /// creating a proof of `J != R.`
    pub const fn join_left<J>(self: TypeNe<L, R>, _eq: TypeEq<J, L>) -> TypeNe<J, R> {
        // SAFETY: (L != R, J == L) implies J != R
        unsafe { TypeNe::<J, R>::new_unchecked() }
    }

    /// Joins a proof of `L != R` with a proof of `R == J`,
    /// creating a proof of `L != J`.
    pub const fn join_right<J>(self: TypeNe<L, R>, _eq: TypeEq<R, J>) -> TypeNe<L, J> {
        // SAFETY: (L != R, R == J) implies L != J
        unsafe { TypeNe::<L, J>::new_unchecked() }
    }
}

#[cfg_attr(feature = "docsrs", doc(cfg(feature = "inj_type_fn")))]
impl<L0, R0> TypeNe<L0, R0> {
    /// Combines two `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1), (R0, R1)>`.
    pub const fn zip<L1, R1>(
        self: TypeNe<L0, R0>,
        other: TypeNe<L1, R1>,
    ) -> TypeNe<(L0, L1), (R0, R1)> {
        zip_impl!{self[L0, R0], other[L1, R1]}
    }

    /// Combines three `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1, L2), (R0, R1, R2)>`.
    pub const fn zip3<L1, R1, L2, R2>(
        self: TypeNe<L0, R0>,
        other1: TypeNe<L1, R1>,
        other2: TypeNe<L2, R2>,
    ) -> TypeNe<(L0, L1, L2), (R0, R1, R2)> {
        zip_impl!{
            self[L0, R0],
            other1[L1, R1],
            other2[L2, R2],
        }
    }

    /// Combines four `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1, L2, L3), (R0, R1, R2, R3)>`.
    pub const fn zip4<L1, R1, L2, R2, L3, R3>(
        self: TypeNe<L0, R0>,
        other1: TypeNe<L1, R1>,
        other2: TypeNe<L2, R2>,
        other3: TypeNe<L3, R3>,
    ) -> TypeNe<(L0, L1, L2, L3), (R0, R1, R2, R3)> {
        zip_impl!{
            self[L0, R0],
            other1[L1, R1],
            other2[L2, R2],
            other3[L3, R3],
        }
    }
}
