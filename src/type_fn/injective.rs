//! Injective type-level functions


use crate::{CallFn, TypeFn};

use core::marker::PhantomData;


/// An 
/// [injective type-level function](mod@crate::type_fn#injective)
/// 
/// This trait is implemented automatically when both
/// [`TypeFn`] and [`RevTypeFn`] are implemented, and cannot be manually implemented.
/// 
/// # Example
/// 
/// ### Macro-based Implementation
/// 
/// ```rust
/// use typewit::{CallInjFn, UncallFn, inj_type_fn};
/// 
/// let _: CallInjFn<BoxFn, u32> = Box::new(3u32);
/// let _: UncallFn<BoxFn, Box<u32>> = 3u32;
/// 
/// inj_type_fn!{
///     struct BoxFn;
/// 
///     impl<T: ?Sized> T => Box<T>
/// }
/// ```
/// 
/// ### Non-macro Implementation
/// 
/// ```rust
/// use typewit::{CallInjFn, RevTypeFn, TypeFn, UncallFn};
/// 
/// let _: CallInjFn<BoxFn, u32> = Box::new(3u32);
/// let _: UncallFn<BoxFn, Box<u32>> = 3u32;
/// 
/// 
/// struct BoxFn;
///
/// impl<T: ?Sized> TypeFn<T> for BoxFn {
///     type Output = Box<T>;
/// }
/// 
/// impl<T: ?Sized> RevTypeFn<Box<T>> for BoxFn {
///     type Arg = T;
/// }
/// 
/// ```
/// 
pub trait InjTypeFn<A: ?Sized>: TypeFn<A, Output = Self::Ret> + RevTypeFn<Self::Ret, Arg = A> {
    /// Return value of the function
    type Ret: ?Sized;
}

impl<F, A: ?Sized, R: ?Sized> InjTypeFn<A> for F
where
    F: TypeFn<A, Output = R>,
    F: RevTypeFn<R, Arg = A>,
{
    type Ret = R;
}

/// The inverse of [`TypeFn`], 
/// for getting the argument of a [`TypeFn`](crate::type_fn::TypeFn)
/// from its return value.
/// 
/// # Example
/// 
/// ```rust
/// use std::ops::Range;
///
/// use typewit::{RevTypeFn, UncallFn};
///
/// let array = [3usize, 5];
///
/// // Getting the argument of `ArrayFn` from its return value
/// let value: <ArrayFn<2> as RevTypeFn<[usize; 2]>>::Arg = array[0];
/// // more concise way to write the above line
/// let other: UncallFn<ArrayFn<2>, [usize; 2]> = array[0];
///
/// assert_eq!(value, 3usize);
/// assert_eq!(other, 3usize);
///
/// typewit::inj_type_fn!{
///     struct ArrayFn<const N: usize>;
///     impl<T> T => [T; N]
/// }
/// ```
/// 
pub trait RevTypeFn<Ret: ?Sized>: TypeFn<Self::Arg, Output = Ret> {
    /// The argument to this function with `Ret` as the return value.
    type Arg: ?Sized;
}

/// Queries the argument to a `F: `[`TypeFn`] from its return value.
/// 
/// # Example
/// 
/// ```rust
/// use typewit::UncallFn;
/// 
/// let vect = vec![3u32, 5, 8];
/// let value: UncallFn<VecFn, Vec<u32>> = vect[1];
/// assert_eq!(value, 5u32);
/// 
/// typewit::inj_type_fn!{
///     struct VecFn;
///     impl<T> T => Vec<T>
/// }
/// ```
pub type UncallFn<F, Ret> = <F as RevTypeFn<Ret>>::Arg;


/// [`CallFn`] with an additional `F:`[`InjTypeFn<A>`] requirement.
pub type CallInjFn<F, A> = <F as InjTypeFn<A>>::Ret;


macro_rules! simple_inj_type_fn {
    (
        impl[$($impl:tt)*] ($arg:ty => $ret:ty) for $func:ty
        $(where[$($where:tt)*])?
    ) => {
        impl<$($impl)*> crate::type_fn::TypeFn<$arg> for $func
        $(where $($where)*)?
        {
            type Output = $ret;
        }

        impl<$($impl)*> crate::type_fn::RevTypeFn<$ret> for $func
        $(where $($where)*)?
        {
            type Arg = $arg;
        }
    };
} pub(crate) use simple_inj_type_fn;