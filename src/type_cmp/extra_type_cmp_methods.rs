use crate::type_fn::{InjTypeFn, InvokeAlias, CallInjFn, UncallFn};

#[cfg(feature = "alloc")]
use alloc::boxed::Box;


/// 
/// # Why `InjTypeFn`
/// 
/// Both [`map`](Self::map) and [`project`](Self::project) 
/// require that the function is [injective]
/// so that `TypeCmp`'s arguments don't change from
/// being equal to unequal or viceversa.
/// 
/// [injective]: mod@crate::type_fn#injective
/// 
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "inj_type_fn")))]
impl<L: ?Sized, R: ?Sized> TypeCmp<L, R> {
    /// Maps the type arguments of this `TypeCmp`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`project`](Self::project) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    pub const fn map<F>(
        self: TypeCmp<L, R>,
        func: F,
    ) -> TypeCmp<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        match self {
            TEq(te) => TEq(te.map::<F>(func)),
            TNe(te) => TNe(te.map::<F>(func)),
        }
    }

    /// Maps the type arguments of this `TypeCmp`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`map`](Self::map) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    pub const fn project<F>(
        self: TypeCmp<L, R>,
    ) -> TypeCmp<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        match self {
            TEq(te) => TEq(te.project::<F>()),
            TNe(te) => TNe(te.project::<F>()),
        }
    }

    /// Maps the type arguments of this `TypeCmp`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unproject`](Self::unproject) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    pub const fn unmap<F>(
        self,
        func: F,
    ) -> TypeCmp<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        match self {
            TEq(te) => TEq(te.unmap::<F>(func)),
            TNe(te) => TNe(te.unmap::<F>(func)),
        }
    }
    /// Maps the type arguments of this `TypeCmp`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unmap`](Self::unmap) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    pub const fn unproject<F>(
        self,
    ) -> TypeCmp<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        match self {
            TEq(te) => TEq(te.unproject::<F>()),
            TNe(te) => TNe(te.unproject::<F>()),
        }
    }

    /// Converts a `TypeCmp<L, R>` to `TypeCmp<&L, &R>`
    pub const fn in_ref<'a>(self) -> TypeCmp<&'a L, &'a R> {
        match self {
            TEq(te) => TEq(te.in_ref()),
            TNe(te) => TNe(te.in_ref()),
        }
    }

    crate::utils::conditionally_const!{
        feature = "mut_refs";

        /// Converts a `TypeCmp<L, R>` to `TypeCmp<&mut L, &mut R>`
        /// 
        /// # Constness
        /// 
        /// This requires either of the `"mut_refs"` or `"const_mut_refs"` 
        /// crate features to be enabled to be a `const fn`.
        /// 
        pub fn in_mut['a](self) -> TypeCmp<&'a mut L, &'a mut R> {
            match self {
                TEq(te) => TEq(te.in_mut()),
                TNe(te) => TNe(te.in_mut()),
            }
        }
    }

    /// Converts a `TypeCmp<L, R>` to `TypeCmp<Box<L>, Box<R>>`
    #[cfg(feature = "alloc")]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
    pub const fn in_box(self) -> TypeCmp<Box<L>, Box<R>> {
        match self {
            TEq(te) => TEq(te.in_box()),
            TNe(te) => TNe(te.in_box()),
        }
    }
}