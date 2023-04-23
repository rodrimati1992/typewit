//! Type-level functions.

use core::marker::PhantomData;

/// A function that operates purely on the level of types.
/// 
/// These can be used in `typewit` to 
/// [map the type arguments of `TypeEq`](crate::TypeEq::project).
/// 
/// # Example
/// 
/// ```rust
/// use typewit::{TypeFn, CallFn};
/// 
/// let string: CallFn<AddOutput<String>, &str> = "foo".to_string() +  ", bar";
/// let _: String = string;
/// assert_eq!(string, "foo, bar");
/// 
/// 
/// struct AddOutput<Lhs>(core::marker::PhantomData<Lhs>);
/// 
/// // This part is optional,
/// // only necessary to pass the function as a value, not just as a type.
/// impl<Lhs> AddOutput<Lhs> {
///     const NEW: Self = Self(core::marker::PhantomData);
/// }
/// 
/// impl<Lhs, Rhs> TypeFn<Rhs> for AddOutput<Lhs>
/// where
///     Lhs: core::ops::Add<Rhs>
/// {
///     type Output = Lhs::Output;
/// }
/// 
/// ```
pub trait TypeFn<T: ?Sized> {
    /// The return value of the function
    type Output: ?Sized;
}

/// Allows functions to be used as `TypeFn`s.
/// 
/// Not too useful until a single closure value can 
/// take many different types as arguments,
/// e.g: `a_closure("bar"); a_closure(3);`.
impl<F, T, R> TypeFn<T> for F 
where
    F: Fn(T) -> R
{
    type Output = R;
}

/// Calls the `F` [type-level function](TypeFn) with `T` as its argument.
/// 
/// # Example
/// 
/// ```rust
/// use typewit::type_fn::{CallFn, GRef, TypeFn};
/// 
/// assert_eq!(mul(3u8, &5u8), 15u8);
/// 
/// fn mul<L, R>(l: L, r: R) -> CallFn<MulOutput<L>, R> 
/// where
///     L: core::ops::Mul<R>
/// {
///     l * r
/// }
/// 
/// struct MulOutput<Lhs>(core::marker::PhantomData<Lhs>);
/// 
/// impl<Lhs, Rhs> TypeFn<Rhs> for MulOutput<Lhs>
/// where
///     Lhs: core::ops::Mul<Rhs>
/// {
///     type Output = Lhs::Output;
/// }
/// 
/// ```
/// 
pub type CallFn<F, T> = <F as TypeFn<T>>::Output;

///////////////////////////////////////////////////////

/// Type-level function from `T` to `&'a T`
pub struct GRef<'a>(PhantomData<fn() -> &'a ()>);

impl<'a> GRef<'a> {
    /// Make a value of this type-level function
    pub const NEW: Self = Self(PhantomData);
}

impl<'a, T: 'a + ?Sized> TypeFn<T> for GRef<'a> {
    type Output = &'a T;
}

////////////////

/// Type-level function from `T` to `&'a mut T`
pub struct GRefMut<'a>(PhantomData<fn() -> &'a mut ()>);

impl<'a> GRefMut<'a> {
    /// Make a value of this type-level function
    pub const NEW: Self = Self(PhantomData);
}

impl<'a, T: 'a + ?Sized> TypeFn<T> for GRefMut<'a> {
    type Output = &'a mut T;
}

////////////////

/// Type-level function from `T` to `Box<T>`
#[cfg(feature = "alloc")]
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "alloc")))]
pub struct GBox;

#[cfg(feature = "alloc")]
impl<T: ?Sized> TypeFn<T> for GBox {
    type Output = alloc::boxed::Box<T>;
}

