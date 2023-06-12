use crate::TypeEq;

/// Emulation of `T == U` bounds.
/// 
/// This trait emulates `T == U` bounds with `T: Identity<Type = U>`.
/// 
/// # Example
/// 
/// (this example requires Rust 1.61.0, because it uses trait bounds in a `const fn`)
/// 
#[cfg_attr(not(feature = "rust_1_61"), doc = "```ignore")]
#[cfg_attr(feature = "rust_1_61", doc = "```rust")]
/// use typewit::{Identity, TypeEq};
/// 
/// assert_eq!(foo(3), [3, 3]);
/// 
/// assert_eq!(foo::<&str, 2, _>("hello"), ["hello", "hello"]);
///
///
/// const fn foo<T, const N: usize, R>(val: T) -> R
/// where
///     // emulates a `[T; N] == R` bound
///     [T; N]: Identity<Type = R>,
///     T: Copy,
/// {
///     Identity::TYPE_EQ // returns a `TypeEq<[T; N], R>`
///         .to_right([val; N]) // casts `[T; N]` to `R`
/// }
/// ```
/// 
pub trait Identity {
    /// The same type as `Self`,
    /// used to emulate type equality bounds (`T == U`)
    /// with associated type equality constraints
    /// (`T: Identity<Type = U>`).
    type Type: ?Sized;
    
    /// Proof that `Self` is the same type as `Self::Type`,
    /// provides methods for casting between `Self` and `Self::Type`.
    const TYPE_EQ: TypeEq<Self, Self::Type>;
}

impl<T: ?Sized> Identity for T {
    type Type = T;

    const TYPE_EQ: TypeEq<Self, Self::Type> = TypeEq::NEW;
}
