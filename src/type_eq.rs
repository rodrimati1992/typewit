use crate::type_fn::{self, TypeFn, CallFn};

use core::{
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    default::Default,
    hash::{Hash, Hasher},
    fmt::{self, Debug},
    mem::forget,
};

#[cfg(feature = "alloc")]
use alloc::boxed::Box;


macro_rules! projected_type_eq {
    ($type_eq:ident, $Left:ident, $Right:ident, $generic:ty) => {unsafe{
        let _te: crate::TypeEq<$Left, $Right> = $type_eq;
        crate::__ProjectedTypeEq::<$generic, $Left, $Right>::new_unchecked()
    }}
}

/// A [`TypeEq`] whose type arguments are projected using the 
/// `F` [type-level function](crate::type_fn::TypeFn).
#[doc(hidden)]
pub type __ProjectedTypeEq<F, L, R> = TypeEq<CallFn<F, L>, CallFn<F, R>>;


// Declaring `TypeEq` in a submodule to prevent "safely" constructing `TypeEq` with
// two different type arguments in the `crate::type_eq` module.
mod type_eq_ {
    use core::marker::PhantomData;

    /// Value-level proof that `L` is the same type as `R`
    /// 
    ///
    /// This type can be used to prove that `L` and `R` are the same type,
    /// because it can only be safely constructed with 
    /// [`TypeEq::<L, L>::NEW`](#associatedconstant.NEW)(or [`new`](#method.new)),
    /// where both type arguments are the same type.
    ///
    /// This type is not too useful by itself, it becomes useful 
    /// [when put inside of an enum](#polymorphic-function).
    ///
    /// # Soundness
    /// 
    /// `TypeEq<L, R>` requires both type arguments to be the same type so that 
    /// [projecting](Self::project) the type arguments results in the same type for 
    /// both arguments.
    /// 
    /// Creating a `TypeEq<L, R>` where `L != R` allows
    /// [transmuting between any two types](#arbitrary-transmute)
    /// (that is bad).
    ///
    /// # Examples
    /// 
    /// ### Polymorphic function
    /// 
    /// This demonstrates how one can write a polymorphic `const fn`
    /// (as of 2023-04-30, trait methods can't be called in const fns)
    /// 
    /// ```rust
    /// use typewit::{HasTypeWitness, MakeTypeWitness, TypeWitnessTypeArg, TypeEq};
    /// 
    /// assert_eq!(returnal::<u8>(), 3);
    /// assert_eq!(returnal::<&str>(), "hello");
    /// 
    /// 
    /// const fn returnal<'a, R>() -> R
    /// where
    ///     R: HasTypeWitness<RetWitness<'a, R>>
    /// {
    ///     // `WITNESS` comes from the `HasTypeWitness` trait        
    ///     match R::WITNESS {
    ///         RetWitness::U8(te) => te.to_left(3u8),
    ///         RetWitness::Str(te) => te.to_right("hello"),
    ///     }
    /// }   
    /// 
    /// enum RetWitness<'a, R> {
    ///     U8(TypeEq<R, u8>),
    ///     Str(TypeEq<&'a str, R>),
    /// }
    /// 
    /// impl<R> TypeWitnessTypeArg for RetWitness<'_, R> {
    ///     type Arg = R;
    /// }
    /// 
    /// impl MakeTypeWitness for RetWitness<'_, u8> {
    ///     const MAKE: Self = RetWitness::U8(TypeEq::NEW);
    /// }
    /// 
    /// impl<'a> MakeTypeWitness for RetWitness<'a, &'a str> {
    ///     const MAKE: Self = RetWitness::Str(TypeEq::NEW);
    /// }
    /// 
    /// ```
    pub struct TypeEq<L: ?Sized, R: ?Sized>(PhantomData<(
        fn(PhantomData<L>) -> PhantomData<L>,
        fn(PhantomData<R>) -> PhantomData<R>,
    )>);

    impl<L: ?Sized> TypeEq<L, L> {
        /// Constructs a `TypeEq<L, L>`.
        pub const NEW: Self = TypeEq(PhantomData);
    }

    impl TypeEq<(), ()> {
        /// Constructs a `TypeEq<T, T>`.
        #[inline(always)]
        pub const fn new<T: ?Sized>() -> TypeEq<T, T> {
            TypeEq::<T, T>::NEW
        }
    }

    impl<L: ?Sized, R: ?Sized> TypeEq<L, R> {
        /// Swaps the type parameters of this `TypeEq`
        #[inline(always)]
        pub const fn flip(self) -> TypeEq<R, L> {
            TypeEq(PhantomData)
        }

        /// Constructs a `TypeEq<L, R>`.
        ///
        /// # Safety
        ///
        /// You must ensure that `L` is the same type as `R`.
        ///
        /// # Examples
        ///
        /// ### Unsound usage
        /// <span id="arbitrary-transmute"></span>
        ///
        /// This example demonstrates why `L == R` is a strict requirement.
        ///
        /// ```rust
        /// use typewit::{TypeEq, TypeFn};
        ///
        /// // SAFETY: WRONG! UNSOUND!
        /// let te: TypeEq<u8, i8> = unsafe{ TypeEq::new_unchecked() };
        /// 
        /// // because `TypeEq<u8, i8>` is incorrect,
        /// // we get this absurd `TypeEq` from the `project` method.
        /// let absurd: TypeEq<(), Vec<usize>> = te.project::<Func>();
        /// 
        /// // This cast is UB, it killed the test runner when uncommented.
        /// // absurd.to_right(()); 
        /// 
        /// struct Func;
        /// impl TypeFn<u8> for Func { type Output = (); }
        /// impl TypeFn<i8> for Func { type Output = Vec<usize>; }
        ///
        ///
        /// ```
        ///
        #[inline(always)]
        pub const unsafe fn new_unchecked() -> TypeEq<L, R> {
            TypeEq(PhantomData)
        }
    }
}
pub use type_eq_::TypeEq;

impl<L: ?Sized, R: ?Sized> Copy for TypeEq<L, R> {}

impl<L: ?Sized, R: ?Sized> Clone for TypeEq<L, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<L, R> TypeEq<L, R> {
    /// Whether `L` is the same type as `R`.
    /// 
    /// False positive equality is fine for this associated constant,
    /// since it's used to optimize out definitely unequal types.
    const ARE_SAME_TYPE: Amb = {
        // hacky way to emulate a lifetime-unaware
        // `TypeId::of<L>() == TypeId::of<R>()`
        let approx_same_type = {
            core::mem::size_of::<L>() == core::mem::size_of::<R>()
                && core::mem::align_of::<L>() == core::mem::align_of::<R>()
                && core::mem::size_of::<Option<L>>() == core::mem::size_of::<Option<R>>()
                && core::mem::align_of::<Option<L>>() == core::mem::align_of::<Option<R>>()
        };

        if approx_same_type {
            Amb::Indefinite
        } else {
            Amb::No
        }
    };

    /// Hints to the compiler that a `TypeEq<L, R>`
    /// can only be constructed if `L == R`.
    ///
    /// This function takes and returns `val` unmodified.
    /// This allows returning some value from an expression
    /// while hinting that `L == R`.
    ///
    #[inline(always)]
    pub const fn reachability_hint<T>(self, val: T) -> T {
        if let Amb::No = Self::ARE_SAME_TYPE {
            // safety: it's impossible to have a `TypeEq<L, R>` value
            // where `L` and `R` are not the same type
            unsafe { core::hint::unreachable_unchecked() }
        }

        val
    }

    /// A no-op cast from `L` to `R`.
    /// 
    /// This cast is a no-op because having a `TypeEq<L, R>` value
    /// proves that `L` and `R` are the same type.
    #[inline(always)]
    pub const fn to_right(self, from: L) -> R {
        self.reachability_hint(());

        unsafe { crate::__priv_transmute!(L, R, from) }
    }
    /// A no-op cast from `R` to `L`.
    /// 
    /// This cast is a no-op because having a `TypeEq<L, R>` value
    /// proves that `L` and `R` are the same type.
    #[inline(always)]
    pub const fn to_left(self, from: R) -> L {
        self.reachability_hint(());

        unsafe { crate::__priv_transmute!(R, L, from) }
    }
}

impl<L: ?Sized, R: ?Sized> TypeEq<L, R> {
    /// Maps the type arguments of this `TypeEq`
    /// by using the `F` [type-level function](crate::type_fn::TypeFn).
    /// 
    /// Use this function over [`project`](Self::project) 
    /// if you want the type of the passed in function to be inferred.
    ///
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeEq, TypeFn};
    /// 
    /// assert_eq!(foo(TypeEq::NEW), (false, 5));
    /// 
    /// const fn foo<'a, T>(te: TypeEq<u32, T>) -> (bool, T) {
    ///     // The argument passed to `TypeEq::map` is a `GPair<bool>`
    ///     // `GPair<bool>` maps `u32` to `(bool, u32)`
    ///     //           and maps `T`   to `(bool, T)`
    ///     let map_te: TypeEq<(bool, u32), (bool, T)> = te.map(GPair::NEW); 
    /// 
    ///     map_te.to_right((false, 5u32))
    /// }
    /// 
    /// // `GPair<A>` is a type-level function from `B` to `(A, B)` 
    /// struct GPair<A>(std::marker::PhantomData<fn() -> A>);
    /// 
    /// impl<A> GPair<A> { 
    ///     const NEW: Self = Self(std::marker::PhantomData); 
    /// }
    /// 
    /// // what makes GPair a type-level function
    /// impl<A, B> TypeFn<B> for GPair<A> {
    ///     type Output = (A, B);
    /// }
    /// 
    /// ```
    /// 
    pub const fn map<F>(self, func: F) -> TypeEq<CallFn<F, L>, CallFn<F, R>>
    where
        F: TypeFn<L> + TypeFn<R>
    {
        forget(func);
        projected_type_eq!{self, L, R, F}
    }

    /// Maps the type arguments of this `TypeEq`
    /// by using the `F` [type-level function](crate::type_fn::TypeFn).
    /// 
    /// Use this function over [`map`](Self::map) 
    /// if you want to specify the type of the passed in function explicitly.
    /// 
    /// # Example
    /// 
    /// ```rust
    /// use typewit::{TypeEq, TypeFn};
    /// 
    /// assert_eq!(foo(TypeEq::NEW), vec![3u32, 5, 8]);
    /// 
    /// fn foo<T>(te: TypeEq<u32, T>) -> Vec<T> {
    ///     let vec_te: TypeEq<Vec<u32>, Vec<T>> = te.project::<GVec>();
    ///     vec_te.to_right(vec![3, 5, 8])
    /// }
    /// 
    /// struct GVec;
    /// 
    /// impl<T> TypeFn<T> for GVec {
    ///     type Output = Vec<T>;
    /// }
    /// 
    /// ```
    /// 
    pub const fn project<F>(self) -> TypeEq<CallFn<F, L>, CallFn<F, R>>
    where
        F: TypeFn<L> + TypeFn<R>
    {
        projected_type_eq!{self, L, R, F}
    }

    /// Converts a `TypeEq<L, R>` to `TypeEq<&L, &R>`
    pub const fn in_ref<'a>(self) -> TypeEq<&'a L, &'a R> {
        projected_type_eq!{self, L, R, type_fn::GRef<'a>}
    }

    crate::utils::conditionally_const!{
        feature = "mut_refs";

        /// Converts a `TypeEq<L, R>` to `TypeEq<&mut L, &mut R>`
        /// 
        /// # Constness
        /// 
        /// This requires either of the `"mut_refs"` or `"const_mut_refs"` 
        /// crate features to be enabled to be a `const fn`.
        pub fn in_mut['a](self) -> TypeEq<&'a mut L, &'a mut R> {
            projected_type_eq!{self, L, R, type_fn::GRefMut<'a>}
        }
    }

    /// Converts a `TypeEq<L, R>` to `TypeEq<Box<L>, Box<R>>`
    #[cfg(feature = "alloc")]
    #[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
    pub const fn in_box(self) -> TypeEq<Box<L>, Box<R>> {
        projected_type_eq!{self, L, R, type_fn::GBox}
    }
}


enum Amb {
    // indefinitely false/true
    Indefinite,
    // definitely false
    No,
}



impl<T: ?Sized> Default for TypeEq<T, T> {
    fn default() -> Self {
        Self::NEW
    }
}

impl<L: ?Sized, R: ?Sized> Debug for TypeEq<L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TypeEq")
    }
}

impl<L: ?Sized, R: ?Sized> PartialEq for TypeEq<L, R> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<L: ?Sized, R: ?Sized> PartialOrd for TypeEq<L, R> {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        Some(Ordering::Equal)
    }
}

impl<L: ?Sized, R: ?Sized> Ord for TypeEq<L, R> {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl<L: ?Sized, R: ?Sized> Eq for TypeEq<L, R> {}


impl<L: ?Sized, R: ?Sized> Hash for TypeEq<L, R> {
    fn hash<H>(&self, state: &mut H)
    where H: Hasher
    {
        ().hash(state)
    }
}

