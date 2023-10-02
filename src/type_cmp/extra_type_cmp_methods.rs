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
            TypeCmp::Eq(te) => TypeCmp::Eq(te.map::<F>(func)),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.map::<F>(func)),
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
            TypeCmp::Eq(te) => TypeCmp::Eq(te.project::<F>()),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.project::<F>()),
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
            TypeCmp::Eq(te) => TypeCmp::Eq(te.unmap::<F>(func)),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.unmap::<F>(func)),
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
            TypeCmp::Eq(te) => TypeCmp::Eq(te.unproject::<F>()),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.unproject::<F>()),
        }
    }

    /// Converts a `TypeCmp<L, R>` to `TypeCmp<&L, &R>`
    pub const fn in_ref<'a>(self) -> TypeCmp<&'a L, &'a R> {
        match self {
            TypeCmp::Eq(te) => TypeCmp::Eq(te.in_ref()),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.in_ref()),
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
                TypeCmp::Eq(te) => TypeCmp::Eq(te.in_mut()),
                TypeCmp::Ne(te) => TypeCmp::Ne(te.in_mut()),
            }
        }
    }

    /// Converts a `TypeCmp<L, R>` to `TypeCmp<Box<L>, Box<R>>`
    #[cfg(feature = "alloc")]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
    pub const fn in_box(self) -> TypeCmp<Box<L>, Box<R>> {
        match self {
            TypeCmp::Eq(te) => TypeCmp::Eq(te.in_box()),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.in_box()),
        }
    }
}



#[cfg(all(feature = "generic_fns", feature = "const_marker"))]
use crate::const_marker::Usize;

#[cfg(all(feature = "generic_fns", feature = "const_marker"))]
#[cfg_attr(
    feature = "docsrs",
    doc(cfg(all(feature = "generic_fns", feature = "const_marker")))
)]
impl<L, R> TypeCmp<L, R> {
    /// Combines `TypeCmp<L, R>` and a
    /// `O:`[`BaseTypeWitness`]`<L = Usize<UL>, R = Usize<UR>>`
    /// into `TypeCmp<[L; UL], [R; UR]>`
    /// 
    /// [`BaseTypeWitness`]: crate::BaseTypeWitness
    /// 
    #[doc = alternative_docs!("in_array")]
    /// 
    /// # Example
    /// 
    /// ### Basic
    /// 
    /// ```rust
    /// use typewit::{
    ///     base_type_wit::in_array,
    ///     const_marker::Usize,
    ///     TypeCmp, TypeEq, TypeNe,
    /// };
    /// 
    /// let cmp_eq_ty: TypeCmp<i32, i32> = TypeCmp::with_any();
    /// let cmp_ne_ty: TypeCmp<i64, u64> = TypeCmp::with_any();
    /// 
    /// let eq_len: TypeEq<Usize<0>, Usize<0>> = TypeEq::NEW;
    /// let ne_len: TypeNe<Usize<1>, Usize<2>> = Usize.equals(Usize).unwrap_ne();
    /// let cmp_eq_len: TypeCmp<Usize<3>, Usize<3>> = Usize.equals(Usize);
    /// let cmp_ne_len: TypeCmp<Usize<5>, Usize<8>> = Usize.equals(Usize);
    /// 
    /// assert!(matches!(cmp_eq_ty.in_array(eq_len), TypeCmp::<[i32; 0], [i32; 0]>::Eq(_)));
    /// assert!(matches!(cmp_eq_ty.in_array(ne_len), TypeCmp::<[i32; 1], [i32; 2]>::Ne(_)));
    /// assert!(matches!(cmp_eq_ty.in_array(cmp_eq_len), TypeCmp::<[i32; 3], [i32; 3]>::Eq(_)));
    /// assert!(matches!(cmp_eq_ty.in_array(cmp_ne_len), TypeCmp::<[i32; 5], [i32; 8]>::Ne(_)));
    /// 
    /// assert!(matches!(cmp_ne_ty.in_array(eq_len), TypeCmp::<[i64; 0], [u64; 0]>::Ne(_)));
    /// assert!(matches!(cmp_ne_ty.in_array(ne_len), TypeCmp::<[i64; 1], [u64; 2]>::Ne(_)));
    /// assert!(matches!(cmp_ne_ty.in_array(cmp_eq_len), TypeCmp::<[i64; 3], [u64; 3]>::Ne(_)));
    /// assert!(matches!(cmp_ne_ty.in_array(cmp_ne_len), TypeCmp::<[i64; 5], [u64; 8]>::Ne(_)));
    /// ```
    pub const fn in_array<O, const UL: usize, const UR: usize>(
        self,
        other: O,
    ) -> TypeCmp<[L; UL], [R; UR]> 
    where
        O: BaseTypeWitness<L = Usize<UL>, R = Usize<UR>>
    {
        use crate::type_fn::PairToArrayFn as PTAF;

        let other = MetaBaseTypeWit::to_cmp(O::WITNESS, other);

        match (self, other) {
            (TypeCmp::Eq(tel), TypeCmp::Eq(ter)) => {
                TypeCmp::Eq(tel.in_array(ter))
            }
            (TypeCmp::Ne(ne), _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::A(TypeEq::NEW).zip2(ne, other).project::<PTAF>())
            }
            (_, TypeCmp::Ne(ne)) => {
                TypeCmp::Ne(SomeTypeArgIsNe::B(TypeEq::NEW).zip2(self, ne).project::<PTAF>())
            }
        }
    }
}