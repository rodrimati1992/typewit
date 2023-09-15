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

