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

////////////////

/// Type-level identity function
pub struct FnIdentity;

impl<T: ?Sized> TypeFn<T> for FnIdentity {
    type Output = T;
}

////////////////

/// Type-level function which implements `TypeFn` by delegating to `F` 
/// 
/// This is mostly a workaround to write `F: TypeFn<T>` bounds in Rust 1.57.0
/// (trait bounds in `const fn`s were stabilized in Rust 1.61.0).
///
/// Because `Foo<F>: Trait`-style bounds unintentionally work in 1.57.0,
/// this crate uses `Invoke<F>: TypeFn<T>` 
/// when the `"rust_1_61"` feature is disabled,
/// and `F: TypeFn<T>` when it is enabled.
/// 
pub struct Invoke<F>(PhantomData<fn() -> F>);

impl<F, T: ?Sized> TypeFn<T> for Invoke<F> 
where
    F: TypeFn<T>
{
    type Output = CallFn<F, T>;
}


// This type alias makes it so that docs for newer Rust versions don't
// show `Invoke<F>`, keeping the method bounds the same as in 1.0.0.
#[cfg(not(feature = "rust_1_61"))]
pub(crate) type InvokeAlias<F> = Invoke<F>;

#[cfg(feature = "rust_1_61")]
pub(crate) type InvokeAlias<F> = F;