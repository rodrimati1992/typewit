//! Type-level functions.

use core::marker::PhantomData;

/// A function that operates purely on the level of types.
/// 
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

/// Calls the `F` [`TypeFn`] with `T` as its argument.
pub type CallFn<F, T> = <F as TypeFn<T>>::Output;

///////////////////////////////////////////////////////

/// Type-level function from `T` to `&'a T`
pub struct GRef<'a>(PhantomData<fn() -> &'a ()>);

impl<'a> GRef<'a> {
    pub const NEW: Self = Self(PhantomData);
}

impl<'a, T: 'a + ?Sized> TypeFn<T> for GRef<'a> {
    type Output = &'a T;
}

////////////////

/// Type-level function from `T` to `&'a mut T`
pub struct GRefMut<'a>(PhantomData<fn() -> &'a mut ()>);

impl<'a> GRefMut<'a> {
    pub const NEW: Self = Self(PhantomData);
}

impl<'a, T: 'a + ?Sized> TypeFn<T> for GRefMut<'a> {
    type Output = &'a mut T;
}

