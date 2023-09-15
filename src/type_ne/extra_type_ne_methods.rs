use crate::{
    type_fn::{InjTypeFn, InvokeAlias, CallInjFn, UncallFn},
    TypeEq,
    TypeNe,
};

crate::type_eq_ne_guts::declare_helpers!{
    $
    TypeNe
    InjTypeFn
    CallInjFn
}




/// 
/// # Why `InjTypeFn`
/// 
/// Both [`map`](Self::map) and [`project`](Self::project) 
/// require that the function is [injective]
/// so that `TypeNe`'s arguments stay unequal.
/// 
/// [injective]: mod@crate::type_fn#injective
/// 
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "inj_type_fn")))]
impl<L: ?Sized, R: ?Sized> TypeNe<L, R> {
    /// Maps the type arguments of this `TypeNe`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`project`](Self::project) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    pub const fn map<F>(
        self: TypeNe<L, R>,
        _func: F,
    ) -> TypeNe<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        core::mem::forget(_func);
        projected_type_cmp!{self, L, R, F}
    }

    /// Maps the type arguments of this `TypeNe`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`map`](Self::map) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    pub const fn project<F>(
        self: TypeNe<L, R>,
    ) -> TypeNe<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        projected_type_cmp!{self, L, R, F}
    }

    /// Maps the type arguments of this `TypeNe`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unproject`](Self::unproject) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    pub const fn unmap<F>(
        self,
        func: F,
    ) -> TypeNe<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        core::mem::forget(func);
        
        unprojected_type_cmp!{self, L, R, F}
    }
    /// Maps the type arguments of this `TypeNe`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unmap`](Self::unmap) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    pub const fn unproject<F>(
        self,
    ) -> TypeNe<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        unprojected_type_cmp!{self, L, R, F}
    }
}
