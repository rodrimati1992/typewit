use crate::{TypeEq, TypeNe};

use core::{
    any::{Any, TypeId},
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
    fmt::{self, Debug},
    mem::discriminant,
};

/// The result of comparing two types for equality.
pub enum TypeCmp<L: ?Sized, R: ?Sized>{
    ///
    TEq(TypeEq<L, R>),
    ///
    TNe(TypeNe<L, R>),
}

use TypeCmp::{TEq, TNe};


impl<L: ?Sized, R: ?Sized> TypeCmp<L, R> {
    /// Constructs a `TypeCmp<L, R>` of types that implement `Any`.
    pub fn with_any() -> Self
    where
        L: Sized + Any,
        R: Sized + Any,
    {
        if let Some(equal) = TypeEq::with_any() {
            TEq(equal)
        } else if let Some(unequal) = TypeNe::with_any() {
            TNe(unequal)
        } else {
            unreachable!()
        }
    }

    /// Swaps the type arguments of this `TypeCmp`
    pub const fn flip(self) -> TypeCmp<R, L> {
        match self {
            TEq(te) => TEq(te.flip()),
            TNe(te) => TNe(te.flip()),
        }
    }

    /// Joins this `TypeCmp<L, R>` with a `TypeEq<Q, L>`, producing a `TypeCmp<Q, R>`.
    pub const fn join_left<Q: ?Sized>(self, left: TypeEq<Q, L>) -> TypeCmp<Q, R> {
        match self {
            TEq(te) => TEq(left.join(te)),
            TNe(te) => TNe(te.join_left(left)),
        }
    }

    /// Joins this `TypeCmp<L, R>` with a `TypeEq<R, Q>`, producing a `TypeCmp<L, Q>`.
    pub const fn join_right<Q: ?Sized>(self, right: TypeEq<R, Q>) -> TypeCmp<L, Q> {
        match self {
            TEq(te) => TEq(te.join(right)),
            TNe(te) => TNe(te.join_right(right)),
        }
    }

    /// Converts this `TypeCmp<L, R>` into an `Option<TypeEq<L, R>>`.
    pub const fn teq(self) -> Option<TypeEq<L, R>> {
        match self {
            TEq(te) => Some(te),
            TNe(_) => None,
        }
    }

    /// Converts this `TypeCmp<L, R>` into an `Option<TypeNe<L, R>>`.
    pub const fn tne(self) -> Option<TypeNe<L, R>> {
        match self {
            TEq(_) => None,
            TNe(te) => Some(te),
        }
    }

    /// Returns whether this `TypeCmp` is a `TEq`.
    pub const fn is_teq(self) -> bool {
        matches!(self, TEq(_))
    }

    /// Returns whether this `TypeCmp` is a `TNe`.
    pub const fn is_tne(self) -> bool {
        matches!(self, TNe(_))
    }

    /// Returns the contained `TypeEq`
    /// 
    /// # Panic
    /// 
    /// Panics if the contained value is a `TypeNe`.
    #[track_caller]
    pub const fn unwrap_teq(self) -> TypeEq<L, R> {
        match self {
            TEq(te) => te,
            TNe(_) => panic!("called `TypeCmp::unwrap_teq` on a `TypeNe` value"),
        }
    }

    /// Returns the contained `TypeNe`
    /// 
    /// # Panic
    /// 
    /// Panics if the contained value is a `TypeEq`.
    #[track_caller]
    pub const fn unwrap_tne(self) -> TypeNe<L, R> {
        match self {
            TEq(_) => panic!("called `TypeCmp::unwrap_tne` on a `TypeEq` value"),
            TNe(te) => te,
        }
    }
}




impl<L: ?Sized, R: ?Sized> Copy for TypeCmp<L, R> {}

impl<L: ?Sized, R: ?Sized> Clone for TypeCmp<L, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<L: ?Sized, R: ?Sized> Debug for TypeCmp<L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TEq(x) => Debug::fmt(x, f),
            TNe(x) => Debug::fmt(x, f),
        }
    }
}

impl<L: ?Sized, R: ?Sized> PartialEq for TypeCmp<L, R> {
    fn eq(&self, other: &Self) -> bool {
        self.is_teq() == other.is_teq()
    }
}

impl<L: ?Sized, R: ?Sized> PartialOrd for TypeCmp<L, R> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.is_teq().partial_cmp(&other.is_teq())
    }
}

impl<L: ?Sized, R: ?Sized> Ord for TypeCmp<L, R> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.is_teq().cmp(&other.is_teq())
    }
}

impl<L: ?Sized, R: ?Sized> Eq for TypeCmp<L, R> {}

impl<L: ?Sized, R: ?Sized> Hash for TypeCmp<L, R> {
    fn hash<H>(&self, state: &mut H)
    where H: Hasher
    {
        match self {
            TEq(x) => Hash::hash(x, state),
            TNe(x) => Hash::hash(x, state),
        }
    }
}








