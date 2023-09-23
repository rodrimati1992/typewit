use crate::{TypeEq, TypeNe};

#[cfg(feature = "generic_fns")]
use crate::base_type_wit::{BaseTypeWitness, MetaBaseTypeWit, SomeTypeArgIsNe};


use core::{
    any::Any,
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
    fmt::{self, Debug},
};

/// The result of comparing two types for equality.
/// 
/// # Example
/// 
/// ### Custom array initialization
/// 
/// (this example requires both the `"const_marker"` and `"inj_type_fn"`, 
/// both enabled by default)
/// 
#[cfg_attr(not(all(feature = "inj_type_fn", feature = "const_marker")), doc = "```ignore")]
#[cfg_attr(all(feature = "inj_type_fn", feature = "const_marker"), doc = "```rust")]
/// use typewit::{const_marker::Usize, TypeCmp, TypeEq, TypeNe};
/// 
/// let empty: [String; 0] = [];
/// assert_eq!(InitArray::<String, 0>::empty().init(), empty);
/// 
/// assert_eq!(InitArray::<u8, 2>::defaulted().init(), [0u8, 0u8]);
/// 
/// assert_eq!(InitArray::with(|i| i.pow(2)).init(), [0usize, 1, 4, 9]);
/// 
/// 
/// enum InitArray<T, const LEN: usize> {
///     NonEmpty(fn(usize) -> T, TypeNe<[T; LEN], [T; 0]>),
///     Empty(TypeEq<[T; LEN], [T; 0]>),
/// }
/// 
/// impl<T, const LEN: usize> InitArray<T, LEN> {
///     pub fn init(self) -> [T; LEN] {
///         match self {
///             InitArray::NonEmpty(func, _) => std::array::from_fn(func),
///             InitArray::Empty(te) => te.to_left([]),
///         }
///     }
/// 
///     pub fn defaulted() -> Self 
///     where
///         T: Default
///     {
///         Self::with(|_| Default::default())
///     }
/// 
///     pub fn with(func: fn(usize) -> T) -> Self {
///         match  Usize::<LEN>.equals(Usize::<0>) // : TypeCmp<Usize<LEN>, Usize<0>>
///             .project::<ArrayFn<T>>() // : TypeCmp<[T; LEN], [T; 0]>
///         {
///             TypeCmp::Ne(ne) => InitArray::NonEmpty(func, ne),
///             TypeCmp::Eq(eq) => InitArray::Empty(eq),
///         }
///     }
/// }
/// 
/// impl<T> InitArray<T, 0> {
///     pub const fn empty() -> Self {
///         Self::Empty(TypeEq::NEW)
///     }
/// }
/// 
/// impl<T, const LEN: usize> Copy for InitArray<T, LEN> {}
/// 
/// impl<T, const LEN: usize> Clone for InitArray<T, LEN> {
///     fn clone(&self) -> Self { *self }
/// }
/// 
/// typewit::inj_type_fn! {
///     // Declares `struct ArrayFn`, which implements `InjTypeFn<Usize<LEN>>`:
///     // an injective type-level function from `Usize<LEN>` to `[T; LEN]`
///     struct ArrayFn<T>;
///     impl<const LEN: usize> Usize<LEN> => [T; LEN]
/// }
/// ```
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "cmp")))]
pub enum TypeCmp<L: ?Sized, R: ?Sized>{
    /// proof of `L == R`
    Eq(TypeEq<L, R>),
    /// proof of `L != R`
    Ne(TypeNe<L, R>),
}

impl<L: ?Sized, R: ?Sized> TypeCmp<L, R> {
    /// Constructs a `TypeCmp<L, R>` of types that implement `Any`.
    pub fn with_any() -> Self
    where
        L: Sized + Any,
        R: Sized + Any,
    {
        if let Some(equal) = TypeEq::with_any() {
            TypeCmp::Eq(equal)
        } else if let Some(unequal) = TypeNe::with_any() {
            TypeCmp::Ne(unequal)
        } else {
            unreachable!()
        }
    }

    /// Swaps the type arguments of this `TypeCmp`
    pub const fn flip(self) -> TypeCmp<R, L> {
        match self {
            TypeCmp::Eq(te) => TypeCmp::Eq(te.flip()),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.flip()),
        }
    }

    /// Joins this `TypeCmp<L, R>` with a `TypeEq<Q, L>`, producing a `TypeCmp<Q, R>`.
    pub const fn join_left<Q: ?Sized>(self, left: TypeEq<Q, L>) -> TypeCmp<Q, R> {
        match self {
            TypeCmp::Eq(te) => TypeCmp::Eq(left.join(te)),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.join_left(left)),
        }
    }

    /// Joins this `TypeCmp<L, R>` with a `TypeEq<R, Q>`, producing a `TypeCmp<L, Q>`.
    pub const fn join_right<Q: ?Sized>(self, right: TypeEq<R, Q>) -> TypeCmp<L, Q> {
        match self {
            TypeCmp::Eq(te) => TypeCmp::Eq(te.join(right)),
            TypeCmp::Ne(te) => TypeCmp::Ne(te.join_right(right)),
        }
    }

    /// Converts this `TypeCmp<L, R>` into an `Option<TypeEq<L, R>>`.
    pub const fn eq(self) -> Option<TypeEq<L, R>> {
        match self {
            TypeCmp::Eq(te) => Some(te),
            TypeCmp::Ne(_) => None,
        }
    }

    /// Converts this `TypeCmp<L, R>` into an `Option<TypeNe<L, R>>`.
    pub const fn ne(self) -> Option<TypeNe<L, R>> {
        match self {
            TypeCmp::Eq(_) => None,
            TypeCmp::Ne(te) => Some(te),
        }
    }

    /// Returns whether this `TypeCmp` is a `TypeCmp::Eq`.
    pub const fn is_eq(self) -> bool {
        matches!(self, TypeCmp::Eq(_))
    }

    /// Returns whether this `TypeCmp` is a `TypeCmp::Ne`.
    pub const fn is_ne(self) -> bool {
        matches!(self, TypeCmp::Ne(_))
    }

    /// Returns the contained `TypeEq`
    /// 
    /// # Panic
    /// 
    /// Panics if the contained value is a `TypeNe`.
    #[track_caller]
    pub const fn unwrap_eq(self) -> TypeEq<L, R> {
        match self {
            TypeCmp::Eq(te) => te,
            TypeCmp::Ne(_) => panic!("called `TypeCmp::unwrap_eq` on a `TypeNe` value"),
        }
    }

    /// Returns the contained `TypeNe`
    /// 
    /// # Panic
    /// 
    /// Panics if the contained value is a `TypeEq`.
    #[track_caller]
    pub const fn unwrap_ne(self) -> TypeNe<L, R> {
        match self {
            TypeCmp::Eq(_) => panic!("called `TypeCmp::unwrap_ne` on a `TypeEq` value"),
            TypeCmp::Ne(te) => te,
        }
    }
}

#[cfg(feature = "generic_fns")]
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "generic_fns")))]
impl<L, R> TypeCmp<L, R> {
    /// Combines this `TypeCmp<L, R>` with a [`BaseTypeWitness`] type to produce a
    /// `TypeCmp<(L, A::L), (R, A::R)>`.
    pub const fn zip<A>(self, other: A) -> TypeCmp<(L, A::L), (R, A::R)> 
    where
        A: BaseTypeWitness,
    {
        let other = MetaBaseTypeWit::to_cmp(A::WITNESS, other);

        match (self, other) {
            (TypeCmp::Eq(tel), TypeCmp::Eq(ter)) => {
                TypeCmp::Eq(tel.zip(ter))
            }
            (TypeCmp::Ne(ne), _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::A(TypeEq::NEW).zip2(ne, other))
            }
            (_, TypeCmp::Ne(ne)) => {
                TypeCmp::Ne(SomeTypeArgIsNe::B(TypeEq::NEW).zip2(self, ne))
            }
        }
    }

    /// Combines this `TypeCmp<L, R>` with two [`BaseTypeWitness`] types to produce a
    /// `TypeCmp<(L, A::L, B::L), (R, A::R, B::R)>`.
    pub const fn zip3<A, B>(self, arg0: A, arg1: B) -> TypeCmp<(L, A::L, B::L), (R, A::R, B::R)> 
    where
        A: BaseTypeWitness,
        A::L: Sized,
        A::R: Sized,
        B: BaseTypeWitness,
    {
        let arg0 = MetaBaseTypeWit::to_cmp(A::WITNESS, arg0);
        let arg1 = MetaBaseTypeWit::to_cmp(B::WITNESS, arg1);

        match (self, arg0, arg1) {
            (TypeCmp::Eq(te0), TypeCmp::Eq(te1), TypeCmp::Eq(te2)) => {
                TypeCmp::Eq(te0.zip3(te1, te2))
            }
            (TypeCmp::Ne(ne), _, _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::A(TypeEq::NEW).zip3(ne, arg0, arg1))
            }
            (_, TypeCmp::Ne(ne), _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::B(TypeEq::NEW).zip3(self, ne, arg1))
            }
            (_, _, TypeCmp::Ne(ne)) => {
                TypeCmp::Ne(SomeTypeArgIsNe::C(TypeEq::NEW).zip3(self, arg0, ne))
            }
        }
    }

    /// Combines this `TypeCmp<L, R>` with three [`BaseTypeWitness`] types to produce a
    /// `TypeCmp<(L, A::L, B::L, C::L), (R, A::R, B::R, C::R)>`.
    pub const fn zip4<A, B, C>(
        self, 
        arg0: A, 
        arg1: B, 
        arg2: C,
    ) -> TypeCmp<(L, A::L, B::L, C::L), (R, A::R, B::R, C::R)> 
    where
        A: BaseTypeWitness,
        A::L: Sized,
        A::R: Sized,
        B: BaseTypeWitness,
        B::L: Sized,
        B::R: Sized,
        C: BaseTypeWitness,
    {
        let arg0 = MetaBaseTypeWit::to_cmp(A::WITNESS, arg0);
        let arg1 = MetaBaseTypeWit::to_cmp(B::WITNESS, arg1);
        let arg2 = MetaBaseTypeWit::to_cmp(C::WITNESS, arg2);

        match (self, arg0, arg1, arg2) {
            (TypeCmp::Eq(te0), TypeCmp::Eq(te1), TypeCmp::Eq(te2), TypeCmp::Eq(te3)) => {
                TypeCmp::Eq(te0.zip4(te1, te2, te3))
            }
            (TypeCmp::Ne(ne), _, _, _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::A(TypeEq::NEW).zip4(ne, arg0, arg1, arg2))
            }
            (_, TypeCmp::Ne(ne), _, _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::B(TypeEq::NEW).zip4(self, ne, arg1, arg2))
            }
            (_, _, TypeCmp::Ne(ne), _) => {
                TypeCmp::Ne(SomeTypeArgIsNe::C(TypeEq::NEW).zip4(self, arg0, ne, arg2))
            }
            (_, _, _, TypeCmp::Ne(ne)) => {
                TypeCmp::Ne(SomeTypeArgIsNe::D(TypeEq::NEW).zip4(self, arg0, arg1, ne))
            }
        }
    }
}


// using this instead of `mod extra_type_cmp_methods;`
// to document the impls in the submodule below the constructors.
#[cfg(feature = "inj_type_fn")]
include!{"./type_cmp/extra_type_cmp_methods.rs"}


impl<L: ?Sized, R: ?Sized> Copy for TypeCmp<L, R> {}

impl<L: ?Sized, R: ?Sized> Clone for TypeCmp<L, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<L: ?Sized, R: ?Sized> Debug for TypeCmp<L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeCmp::Eq(x) => Debug::fmt(x, f),
            TypeCmp::Ne(x) => Debug::fmt(x, f),
        }
    }
}

impl<L: ?Sized, R: ?Sized> PartialEq for TypeCmp<L, R> {
    fn eq(&self, other: &Self) -> bool {
        self.is_eq() == other.is_eq()
    }
}

impl<L: ?Sized, R: ?Sized> PartialOrd for TypeCmp<L, R> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.is_eq().partial_cmp(&other.is_eq())
    }
}

impl<L: ?Sized, R: ?Sized> Ord for TypeCmp<L, R> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.is_eq().cmp(&other.is_eq())
    }
}

impl<L: ?Sized, R: ?Sized> Eq for TypeCmp<L, R> {}

impl<L: ?Sized, R: ?Sized> Hash for TypeCmp<L, R> {
    fn hash<H>(&self, state: &mut H)
    where H: Hasher
    {
        match self {
            TypeCmp::Eq(x) => Hash::hash(x, state),
            TypeCmp::Ne(x) => Hash::hash(x, state),
        }
    }
}








