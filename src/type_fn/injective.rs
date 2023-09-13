//! Injective type-level functions
//! 
//! # Injective
//! 
//! An injective function is any function `f` for which `a != b` implies `f(a) != f(b)`
//! 
//! This crate uses the [`InjTypeFnArg`] and [`InjTypeFnRet`] traits to encode 
//! injective type-level functions. To *use* these functions, it's recommended 
//! using the [`InjTypeFn`]/[`CoInjTypeFn`] traits as bounds, 
//! and the [`CallFn`]/[`UncallFn`] type aliases to call/uncall the functions respectively.
//! 
//! ### TypeFn
//! 
//! The [`TypeFn`] allows implementors to be non-injective.
//! //! 
//! Example non-injective function:
//! ```rust
//! typewit::type_fn!{
//!     struct Foo;
//!     
//!     impl<T> Vec<T> => T;
//!     impl<T> Box<T> => T;
//! }
//! ```
//! `Foo` is *non*-injective because it maps both `Vec<T>` and `Box<T>` to `T`.
//! 
//! 
//! 
//! [`TypeFn`]: crate::type_fn::TypeFn
//! [`CallFn`]: crate::type_fn::CallFn
//! 

use crate::TypeFn;

use core::marker::PhantomData;


/// Makes `F` an injective type-level function from `Self` to `Self::Ret`.
/// 
/// This trait is purely for *defining* injective type-level functions,
/// For *using* type-level function (as opposed to defining them),
/// use either of the [`InjTypeFn`] or [`CoInjTypeFn`] traits.
/// 
/// # Notes for implementors
/// 
/// Both the implementor and `Self::Ret` associated type 
/// must be wrapped in a unary tuple,
/// this is a workaround for
/// ```text
/// struct FooFn;
/// impl<T> InjTypeFnArg<FooFn> for T {...}
/// ```
/// not being allowed.
/// 
/// In order to implement this trait the compiler will require you to implement
/// [`InjTypeFnRet<F, Arg = Self>`](InjTypeFnRet) for [`Self::Ret`],
/// this ensures that the function is injective.
/// 
/// # Example
/// 
/// ### Macro-based Implementation
/// 
/// ```rust
/// use typewit::type_fn::{CallFn, UncallFn, inj_type_fn};
/// 
/// let _: CallFn<BoxFn, u32> = Box::new(3u32);
/// let _: UncallFn<BoxFn, Box<u32>> = 3u32;
/// 
/// inj_type_fn!{
///     struct BoxFn;
/// 
///     impl<T: ?Sized> T => Box<T>
/// }
/// ```
/// 
/// ### Manual Implementation
/// 
/// ```rust
/// use typewit::type_fn::{CallFn, InjTypeFnArg, InjTypeFnRet, UncallFn};
/// 
/// let _: CallFn<BoxFn, u32> = Box::new(3u32);
/// let _: UncallFn<BoxFn, Box<u32>> = 3u32;
/// 
/// 
/// struct BoxFn;
/// 
/// impl<T: ?Sized> InjTypeFnArg<BoxFn> for (T,) {
///     type Ret = (Box<T>,);
/// }
/// 
/// // The same as the InjTypeFnArg impl with
/// // the `Self` and associated type swapped.
/// impl<T: ?Sized> InjTypeFnRet<BoxFn> for (Box<T>,) {
///     type Arg = (T,);
/// }
/// 
/// ```
/// 
pub trait InjTypeFnArg<F> {
    /// The return value of calling `F`.
    type Ret: InjTypeFnRet<F, Arg = Self> + ?Sized;
}

/////////////

/// Required to make `F` an injective type-level function from `Self::Arg` to `Self`.
/// 
/// This trait is purely for *defining* injective type-level functions,
/// For *using* type-level function (as opposed to defining them),
/// use either of the [`InjTypeFn`] or [`CoInjTypeFn`] traits.
/// 
/// # Notes for implementors
/// 
/// Both the implementor and `Self::Arg` associated type 
/// must be wrapped in a unary tuple,
/// this is a workaround for
/// 
/// to work around
/// ```text
/// struct FooFn;
/// impl<T> InjTypeFnRet<FooFn> for T {...}
/// ```
/// not being allowed.
/// 
/// In order to implement this trait the compiler will require you to implement
/// [`InjTypeFnArg<F, Ret = Self>`](InjTypeFnArg) for [`Self::Arg`].
/// this ensures that the function is injective.
/// 
/// # Examples
/// 
/// The [`InjTypeFnArg`] type defines examples of using this trait.
/// 
pub trait InjTypeFnRet<F> {
    /// The argument to `F` with `Self` as the return value.
    type Arg: InjTypeFnArg<F, Ret = Self> + ?Sized;
}

/////////////

mod sealed {
    use core::marker::PhantomData as PD;

    /// Marker type to prevent `InjTypeFn` from being implemented 
    /// outside of `typewit`
    pub struct ImplsInjectiveFn<F: ?Sized, A: ?Sized, R: ?Sized>(
        pub(super) PD<(fn(PD<F>) -> PD<F>, fn(PD<A>) -> PD<A>, fn(PD<R>) -> PD<R>)>,
    );

    /// Marker type to prevent `CoInjTypeFn` from being implemented 
    /// outside of `typewit`
    pub struct ImplsCoInjectiveFn<F: ?Sized, A: ?Sized, R: ?Sized>(
        pub(super) PD<(fn(PD<F>) -> PD<F>, fn(PD<A>) -> PD<A>, fn(PD<R>) -> PD<R>)>,
    );
}

/// An injective type-level function
/// 
/// This trait has a blanket implementation, 
/// and cannot be manually implemented outside of typewit.
/// 
/// 
/// 
/// 
pub trait InjTypeFn<A: ?Sized>: CoInjTypeFn<Self::Ret> + TypeFn<A, Output = Self::Ret> {
    /// The return value of calling `F`.
    type Ret: ?Sized;

    #[doc(hidden)]
    const __SEALED__: sealed::ImplsInjectiveFn<Self, A, Self::Ret>;
}

impl<A: ?Sized, R: ?Sized, F> InjTypeFn<A> for F
where
    (A,): InjTypeFnArg<F, Ret = (R,)>,
    (R,): InjTypeFnRet<F, Arg = (A,)>,
{
    type Ret = R;

    #[doc(hidden)]
    const __SEALED__: sealed::ImplsInjectiveFn<Self, A, Self::Ret> =
        sealed::ImplsInjectiveFn(PhantomData);
}


impl<A: ?Sized, R: ?Sized, F> TypeFn<A> for F
where
    (A,): InjTypeFnArg<F, Ret = (R,)>,
    (R,): InjTypeFnRet<F, Arg = (A,)>,
{
    type Output = R;
}

/////////////

/// The inverse of [`InjTypeFn`], 
/// for getting the argument of an injective type-level function
/// from the return value.
/// 
/// This trait has a blanket implementation, 
/// and cannot be manually implemented outside of typewit.
/// 
/// 
pub trait CoInjTypeFn<R: ?Sized> {
    /// The argument to `F` with `Self` as the return value.
    type Arg: ?Sized;

    #[doc(hidden)]
    const __SEALED__: sealed::ImplsCoInjectiveFn<Self, Self::Arg, R>;
}

/// Queries the argument to the `F` [injective type-level function](InjTypeFn) 
/// from the return value.
/// 
/// # Example
/// 
/// ```rust
/// use typewit::UncallFn;
/// 
/// let vect = vec![3u32, 5, 8];
/// let value: UncallFn<VecFn, Vec<u32>> = vect[0];
/// assert_eq!(value, 3);
/// 
/// typewit::inj_type_fn!{
///     struct VecFn;
///     impl<T> T => Vec<T>
/// }
/// ```
pub type UncallFn<F, R> = <F as CoInjTypeFn<R>>::Arg;

impl<A: ?Sized, R: ?Sized, F> CoInjTypeFn<R> for F
where
    (R,): InjTypeFnRet<F, Arg = (A,)>,
{
    type Arg = A;

    #[doc(hidden)]
    const __SEALED__: sealed::ImplsCoInjectiveFn<Self, Self::Arg, R> =
        sealed::ImplsCoInjectiveFn(PhantomData);
}
