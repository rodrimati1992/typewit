#[cfg(feature = "alloc")]
use alloc::boxed::Box;


use crate::type_fn::{InjTypeFn, InvokeAlias, CallInjFn, UncallFn};

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
impl<L: ?Sized, R: ?Sized> TypeNe<L, R> {
    /// Maps the type arguments of this `TypeNe`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`project`](Self::project) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// const NE: TypeNe<u8, u16> = type_ne!(u8, u16);
    /// 
    /// const N3: TypeNe<[u8; 0], [u16; 0]> = NE.map(ArrayFn::NEW);
    /// 
    /// inj_type_fn!{
    ///     struct ArrayFn<const LEN: usize>;
    ///     
    ///     impl<T> T => [T; LEN]
    /// }
    /// ```
    pub const fn map<F>(
        self: TypeNe<L, R>,
        _func: F,
    ) -> TypeNe<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        core::mem::forget(_func);
        projected_type_cmp!{self, L, R, InvokeAlias<F>}
    }

    /// Maps the type arguments of this `TypeNe`
    /// by using the `F` [injective type-level function](crate::InjTypeFn).
    /// 
    /// Use this function over [`map`](Self::map) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// const NE: TypeNe<u8, u16> = type_ne!(u8, u16);
    /// 
    /// const N3: TypeNe<Vec<u8>, Vec<u16>> = NE.project::<VecFn>();
    /// 
    /// inj_type_fn!{
    ///     struct VecFn;
    ///     
    ///     impl<T> T => Vec<T>
    /// }
    /// ```
    pub const fn project<F>(
        self: TypeNe<L, R>,
    ) -> TypeNe<CallInjFn<InvokeAlias<F>, L>, CallInjFn<InvokeAlias<F>, R>> 
    where
        InvokeAlias<F>: InjTypeFn<L> + InjTypeFn<R>
    {
        projected_type_cmp!{self, L, R, InvokeAlias<F>}
    }

    /// Maps the type arguments of this `TypeNe`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unproject`](Self::unproject) 
    /// if you want the type of the passed in function to be inferred.
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// use std::cmp::Ordering as CmpOrdering;
    /// use std::sync::atomic::Ordering as MemOrdering;
    /// 
    /// const NE: TypeNe<[CmpOrdering], [MemOrdering]> = type_ne!([CmpOrdering], [MemOrdering]);
    /// 
    /// 
    /// const N3: TypeNe<CmpOrdering, MemOrdering> = NE.unmap(SliceFn);
    /// 
    /// inj_type_fn!{
    ///     struct SliceFn;
    ///     
    ///     impl<T> T => [T]
    /// }
    /// ```
    pub const fn unmap<F>(
        self,
        func: F,
    ) -> TypeNe<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        core::mem::forget(func);
        
        unprojected_type_cmp!{self, L, R, InvokeAlias<F>}
    }
    /// Maps the type arguments of this `TypeNe`
    /// by using the [reversed](crate::RevTypeFn) 
    /// version of the `F` type-level function.
    /// 
    /// Use this function over [`unmap`](Self::unmap) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// const NE: TypeNe<Option<()>, Option<bool>> = type_ne!(Option<()>, Option<bool>);
    /// 
    /// const N3: TypeNe<(), bool> = NE.unproject::<OptionFn>();
    /// 
    /// inj_type_fn!{
    ///     struct OptionFn;
    ///     
    ///     impl<T> T => Option<T>
    /// }
    /// ```
    pub const fn unproject<F>(
        self,
    ) -> TypeNe<UncallFn<InvokeAlias<F>, L>, UncallFn<InvokeAlias<F>, R>>
    where
        InvokeAlias<F>: crate::RevTypeFn<L> + crate::RevTypeFn<R>
    {
        unprojected_type_cmp!{self, L, R, InvokeAlias<F>}
    }

    /// Converts a `TypeNe<L, R>` to `TypeNe<&L, &R>`
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// const NE: TypeNe<i32, u32> = type_ne!(i32, u32);
    /// 
    /// let foo: i32 = 3;
    /// let bar: u32 = 5;
    /// 
    /// baz(&foo, &bar, NE.in_ref());
    /// 
    /// const fn baz<'a, T, U>(foo: &'a T, bar: &'a U, _ne: TypeNe<&'a T, &'a U>) {
    ///     // stuff
    /// }
    /// ```
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
        /// # Example
        /// 
        /// ```rust
        /// use typewit::{TypeNe, inj_type_fn, type_ne};
        /// 
        /// const NE: TypeNe<String, Vec<u8>> = type_ne!(String, Vec<u8>);
        /// 
        /// let mut foo: String = "hello".to_string();
        /// let mut bar: Vec<u8> = vec![3, 5, 8];
        /// 
        /// baz(&mut foo, &mut bar, NE.in_mut());
        /// 
        /// fn baz<'a, T, U>(foo: &'a mut T, bar: &'a mut U, _ne: TypeNe<&'a mut T, &'a mut U>) {
        ///     // stuff
        /// }
        /// ```
        pub fn in_mut['a](self) -> TypeNe<&'a mut L, &'a mut R> {
            projected_type_cmp!{self, L, R, crate::type_fn::GRefMut<'a>}
        }
    }

    /// Converts a `TypeNe<L, R>` to `TypeNe<Box<L>, Box<R>>`
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeNe, inj_type_fn, type_ne};
    /// 
    /// use std::num::{NonZeroI8, NonZeroU8};
    /// 
    /// const NE: TypeNe<NonZeroI8, NonZeroU8> = type_ne!(NonZeroI8, NonZeroU8);
    /// 
    /// let foo: NonZeroI8 = NonZeroI8::new(-1).unwrap();
    /// let bar: NonZeroU8 = NonZeroU8::new(1).unwrap();
    /// 
    /// baz(Box::new(foo), Box::new(bar), NE.in_box());
    /// 
    /// fn baz<T, U>(foo: Box<T>, bar: Box<U>, _ne: TypeNe<Box<T>, Box<U>>) {
    ///     // stuff
    /// }
    /// ```
    #[cfg(feature = "alloc")]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
    pub const fn in_box(self) -> TypeNe<Box<L>, Box<R>> {
        projected_type_cmp!{self, L, R, crate::type_fn::GBox}
    }
}

#[cfg(feature = "rust_1_61")]
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "rust_1_61")))]
impl<L: Sized, R: Sized> TypeNe<L, R> {
    /// Combines `TypeNe<L, R>` and a
    /// `O: `[`BaseTypeWitness`]`<L = Usize<UL>, R = Usize<UR>>`
    /// into `TypeNe<[L; UL], [R; UR]>`
    /// 
    /// [`BaseTypeWitness`]: crate::BaseTypeWitness
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{const_marker::Usize, TypeCmp, TypeEq, TypeNe, type_ne};
    /// 
    /// const NE: TypeNe<u8, i8> = type_ne!(u8, i8);
    /// 
    /// const NE_L: TypeNe<Usize<3>, Usize<5>> = Usize::<3>.equals(Usize::<5>).unwrap_ne();
    /// const EQ_L: TypeEq<Usize<8>, Usize<8>> = TypeEq::NEW;
    /// const TC_L: TypeCmp<Usize<13>, Usize<21>> = Usize::<13>.equals(Usize::<21>);
    /// 
    /// let _: TypeNe<[u8; 3], [i8; 5]> = NE.in_array(NE_L);
    /// let _: TypeNe<[u8; 8], [i8; 8]> = NE.in_array(EQ_L);
    /// let _: TypeNe<[u8; 13], [i8; 21]> = NE.in_array(TC_L);
    /// 
    /// ```
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