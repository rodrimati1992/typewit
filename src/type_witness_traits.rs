#[allow(unused_imports)]
use crate::TypeEq;

use core::marker::PhantomData;


/// Gets a [type witness](crate#what-are-type-witnesses) for `Self`.
/// 
#[doc = explain_type_witness!()]
/// 
/// This trait is a helper to write [`W: MakeTypeWitness<Arg = T>`](MakeTypeWitness) 
/// with the `T` and `W` type parameters flipped,
/// most useful in supertrait bounds.
/// 
/// This trait can't be implemented outside of `typewit`.
/// 
/// # Example 
/// 
/// This example shows how one can make a `const fn` that converts both 
/// `&str` and `&[u8]` to `&str`
/// 
/// ```rust
/// use typewit::{HasTypeWitness, TypeWitnessTypeArg, MakeTypeWitness, TypeEq};
/// 
/// fn main() {
///     assert_eq!(str_try_from("hello"), Ok("hello"));
///     
///     assert_eq!(str_try_from(&[b'w', b'o', b'r', b'l', b'd']), Ok("world"));
///     
///     assert_eq!(str_try_from(b"foo bar" as &[_]), Ok("foo bar"));
/// }
/// 
/// pub const fn str_try_from<'a, T, const L: usize>(
///     input: T
/// ) -> Result<&'a str, std::str::Utf8Error>
/// where
///     T: StrTryFrom<'a, L>
/// {
///     // `WITNESS` comes from the `HasTypeWitness` trait
///     match T::WITNESS {
///         StrTryFromWitness::Str(te) => {
///             // `te` (a `TypeEq<T, &'a str>`) allows coercing between `T` and `&'a str`,
///             // because `TypeEq` is a value-level proof that both types are the same.
///             let string: &str = te.to_right(input);
///             Ok(string)
///         }
///         StrTryFromWitness::Bytes(te) => {
///             let bytes: &[u8] = te.to_right(input);
///             std::str::from_utf8(bytes)
///         }
///         StrTryFromWitness::Array(te) => {
///             let slice: &[u8] = te.to_right(input);
///             str_try_from(slice)
///         }
///     }
/// }
/// 
/// 
/// // trait alias pattern
/// pub trait StrTryFrom<'a, const L: usize>: 
///     Copy + HasTypeWitness<StrTryFromWitness<'a, Self, L>> 
/// {}
/// 
/// impl<'a, T, const L: usize> StrTryFrom<'a, L> for T
/// where
///     T: Copy + HasTypeWitness<StrTryFromWitness<'a, T, L>>
/// {}
/// 
/// 
/// // this enum is a type witness (term is explained in the root module)
/// // `#[non_exhausitve]` allows adding more supported types.
/// #[non_exhaustive]
/// pub enum StrTryFromWitness<'a, T, const L: usize> {
///     // This variant requires `T == &'a str`
///     Str(TypeEq<T, &'a str>),
///
///     // This variant requires `T == &'a [u8]`
///     Bytes(TypeEq<T, &'a [u8]>),
///
///     // This variant requires `T == &'a [u8; L]`
///     Array(TypeEq<T, &'a [u8; L]>),
/// }
/// 
/// impl<'a, T, const L: usize> TypeWitnessTypeArg for StrTryFromWitness<'a, T, L> {
///     type Arg = T;
/// }
/// 
/// impl<'a> MakeTypeWitness for StrTryFromWitness<'a, &'a str, 0> {
///     const MAKE: Self = Self::Str(TypeEq::NEW);
/// }
/// 
/// impl<'a> MakeTypeWitness for StrTryFromWitness<'a, &'a [u8], 0> {
///     const MAKE: Self = Self::Bytes(TypeEq::NEW);
/// }
/// 
/// impl<'a, const L: usize> MakeTypeWitness for StrTryFromWitness<'a, &'a [u8; L], L> {
///     const MAKE: Self = Self::Array(TypeEq::NEW);
/// }
/// 
/// ```
pub trait HasTypeWitness<W: TypeWitnessTypeArg<Arg = Self>> {
    /// A constant of the type witness
    const WITNESS: W;

    // prevents dependencies from implementing this trait 
    #[doc(hidden)]
    const __PRIV_KO9Y329U2U: __Priv<Self, W>;
}

impl<T, W> HasTypeWitness<W> for T
where
    T: ?Sized,
    W: MakeTypeWitness<Arg = T>,
{
    const WITNESS: W = W::MAKE;

    #[doc(hidden)]
    const __PRIV_KO9Y329U2U: __Priv<Self, W> = __Priv(PhantomData, PhantomData);
}

#[doc(hidden)]
pub struct __Priv<T: ?Sized, W>(
    PhantomData<fn() -> PhantomData<W>>,
    PhantomData<fn() -> PhantomData<T>>,
);

////////////////////////////////////////////////

/// Gets the type argument that this [type witness](crate#what-are-type-witnesses) witnesses.
/// 
/// [**example shared with `MakeTypeWitness`**](MakeTypeWitness#example)
/// 
#[doc = explain_type_witness!()]
/// 
/// This trait should be implemented generically, 
/// as generic as the type definition of the implementor,
/// doing so will help type inference.
/// 
pub trait TypeWitnessTypeArg {
    /// The type parameter used for type witnesses.
    ///
    /// Usually, enums that implement this trait have
    /// variants with [`TypeEq`]`<`[`Self::Arg`]`, SomeType>` fields.
    type Arg: ?Sized;
}

/// Constructs this [type witness](crate#what-are-type-witnesses).
/// 
#[doc = explain_type_witness!()]
/// 
/// # Example
/// 
/// ```rust
/// use typewit::{TypeWitnessTypeArg, MakeTypeWitness, TypeEq};
/// 
/// const fn default<'a, T, const L: usize>() -> T 
/// where
///     Defaultable<'a, T, L>: MakeTypeWitness<Arg = T>
/// {
///     match MakeTypeWitness::MAKE {
///         Defaultable::I32(te) => te.to_left(3),
///         Defaultable::Bool(te) => te.to_left(true),
///         Defaultable::Str(te) => te.to_left("empty"),
///         Defaultable::Array(te) => te.to_left([5; L]),
///     }
/// }
/// 
/// let number: i32 = default();
/// assert_eq!(number, 3);
/// 
/// let boolean: bool = default();
/// assert_eq!(boolean, true);
/// 
/// let string: &str = default();
/// assert_eq!(string, "empty");
///
/// let array: [u32; 3] = default();
/// assert_eq!(array, [5, 5, 5]);
/// 
/// 
/// // This enum is a type witness (documented in the root module)
/// #[non_exhaustive]
/// enum Defaultable<'a, T, const L: usize> {
///     // This variant requires `T == i32`
///     I32(TypeEq<T, i32>),
///
///     // This variant requires `T == bool`
///     Bool(TypeEq<T, bool>),
///
///     // This variant requires `T == &'a str`
///     Str(TypeEq<T, &'a str>),
///
///     // This variant requires `T == [u32; L]`
///     Array(TypeEq<T, [u32; L]>),
/// }
/// 
/// impl<T, const L: usize> TypeWitnessTypeArg for Defaultable<'_, T, L> {
///     type Arg = T;
/// }
/// 
/// impl MakeTypeWitness for Defaultable<'_, i32, 0> {
///     const MAKE: Self = Self::I32(TypeEq::NEW);
/// }
/// 
/// impl MakeTypeWitness for Defaultable<'_, bool, 0> {
///     const MAKE: Self = Self::Bool(TypeEq::NEW);
/// }
/// 
/// impl<'a> MakeTypeWitness for Defaultable<'a, &'a str, 0> {
///     const MAKE: Self = Self::Str(TypeEq::NEW);
/// }
/// 
/// impl<const L: usize> MakeTypeWitness for Defaultable<'_, [u32; L], L> {
///     const MAKE: Self = Self::Array(TypeEq::NEW);
/// }
/// 
/// ```
/// 
pub trait MakeTypeWitness: TypeWitnessTypeArg {
    /// A constant with the type witness
    const MAKE: Self;
}
