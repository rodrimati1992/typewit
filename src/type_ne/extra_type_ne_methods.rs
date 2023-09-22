#[cfg(feature = "alloc")]
use alloc::boxed::Box;


use crate::type_fn::{InjTypeFn, InvokeAlias, CallInjFn, UncallFn};

#[cfg(feature = "const_marker")]
use crate::const_marker::Usize;


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

    /// Converts a `TypeNe<L, R>` to `TypeNe<&L, &R>`
    pub const fn in_ref<'a>(self) -> TypeNe<&'a L, &'a R> {
        projected_type_cmp!{self, L, R, crate::type_fn::GRef<'a>}
    }

    crate::utils::conditionally_const!{
        feature = "mut_refs";

        /// Converts a `TypeNe<L, R>` to `TypeNe<&mut L, &mut R>`
        /// 
        /// # Constness
        /// 
        /// This requires either of the `"mut_refs"` or `"const_mut_refs"` 
        /// crate features to be enabled to be a `const fn`.
        /// 
        pub fn in_mut['a](self) -> TypeNe<&'a mut L, &'a mut R> {
            projected_type_cmp!{self, L, R, crate::type_fn::GRefMut<'a>}
        }
    }

    /// Converts a `TypeNe<L, R>` to `TypeNe<Box<L>, Box<R>>`
    #[cfg(feature = "alloc")]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
    pub const fn in_box(self) -> TypeNe<Box<L>, Box<R>> {
        projected_type_cmp!{self, L, R, crate::type_fn::GBox}
    }
}

#[cfg(all(feature = "rust_1_61", feature = "const_marker"))]
#[cfg_attr(
    feature = "docsrs",
    doc(cfg(all(feature = "rust_1_61", feature = "const_marker")))
)]
impl<L: Sized, R: Sized> TypeNe<L, R> {
    /// Combines `TypeNe<L, R>` and a
    /// `O: `[`BaseTypeWitness`]`<L = Usize<UL>, R = Usize<UR>>`
    /// into `TypeNe<[L; UL], [R; UR]>`
    /// 
    /// [`BaseTypeWitness`]: crate::BaseTypeWitness
    pub const fn in_array<O, const UL: usize, const UR: usize>(
        self,
        _other: O,
    ) -> TypeNe<[L; UL], [R; UR]> 
    where
        O: BaseTypeWitness<L = Usize<UL>, R = Usize<UR>>
    {
        // SAFETY: `TypeNe<L, R>` implies `[L; UL] != [R; UR]`,
        //         regardless of whether `UL` equals `UR`
        unsafe {
            TypeNe::new_unchecked()
        }
    }
}