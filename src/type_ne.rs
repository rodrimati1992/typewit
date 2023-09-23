use core::{
    any::{Any, TypeId},
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
    fmt::{self, Debug},
};

use crate::{BaseTypeWitness, TypeEq};

#[cfg(feature = "rust_1_61")]
use crate::base_type_wit::SomeTypeArgIsNe;


pub use self::type_ne_::TypeNe;

mod type_ne_ {
    use core::marker::PhantomData;

    /// Value-level proof that `L` is a different type to `R`
    /// 
    /// The opposite of [`TypeEq`](crate::TypeEq).
    pub struct TypeNe<L: ?Sized, R: ?Sized>(PhantomData<TypeNeHelper<L, R>>);

    // Declared to work around this error in old Rust versions:
    // > error[E0658]: function pointers cannot appear in constant functions
    struct TypeNeHelper<L: ?Sized, R: ?Sized>(
        fn(PhantomData<L>) -> PhantomData<L>,
        fn(PhantomData<R>) -> PhantomData<R>,
    );

    impl<L: ?Sized, R: ?Sized> TypeNe<L, R> {
        /// Constructs a `TypeNe<L, R>`.
        ///
        /// # Safety
        ///
        /// You must ensure that `L != R`.
        ///
        #[inline(always)]
        pub const unsafe fn new_unchecked() -> TypeNe<L, R> {
            TypeNe(PhantomData)
        }
    }

}



impl<L: ?Sized, R: ?Sized> TypeNe<L, R> {
    /// Constructs `TypeNe<L, R>` if `L != R`, otherwise returns None.
    pub fn with_any() -> Option<Self>
    where
        L: Sized + Any,
        R: Sized + Any,
    {
        if TypeId::of::<L>() != TypeId::of::<R>() {
            // SAFETY: the two TypeIds compare unequal, so L != R
            unsafe { Some(TypeNe::new_unchecked()) }
        } else {
            None
        }
    }

    /// Swaps the type arguments of this `TypeNe`
    pub const fn flip(self: TypeNe<L, R>) -> TypeNe<R, L> {
        // SAFETY: type inequality is commutative
        unsafe { TypeNe::<R, L>::new_unchecked() }
    }

    /// Joins a proof of `L != R` with a proof of `J == L`,
    /// creating a proof of `J != R`.
    pub const fn join_left<J: ?Sized>(self: TypeNe<L, R>, _eq: TypeEq<J, L>) -> TypeNe<J, R> {
        // SAFETY: (L != R, J == L) implies J != R
        unsafe { TypeNe::<J, R>::new_unchecked() }
    }

    /// Joins a proof of `L != R` with a proof of `R == J`,
    /// creating a proof of `L != J`.
    pub const fn join_right<J: ?Sized>(self: TypeNe<L, R>, _eq: TypeEq<R, J>) -> TypeNe<L, J> {
        // SAFETY: (L != R, R == J) implies L != J
        unsafe { TypeNe::<L, J>::new_unchecked() }
    }
}

#[cfg(feature = "rust_1_61")]
#[cfg_attr(feature = "docsrs", doc(cfg(feature = "rust_1_61")))]
impl<L, R> TypeNe<L, R> {
    /// Combines this `TypeNe<L, R>` with an 
    /// [`A: BaseTypeWitness`](BaseTypeWitness) to produce a
    /// `TypeNe<(L, A::L), (R, A::R)>`.
    pub const fn zip<A>(
        self: TypeNe<L, R>,
        other: A,
    ) -> TypeNe<(L, A::L), (R, A::R)> 
    where
        A: BaseTypeWitness,
    {
        SomeTypeArgIsNe::A(TypeEq::NEW).zip2(self, other)
    }

    /// Combines this `TypeNe<L, R>` with 
    /// two [`BaseTypeWitness`](BaseTypeWitness)es to produce a
    /// `TypeNe<(L, A::L, B::L), (R, A::R, B::R)>`.
    pub const fn zip3<A, B>(
        self: TypeNe<L, R>,
        other1: A,
        other2: B,
    ) -> TypeNe<(L, A::L, B::L), (R, A::R, B::R)> 
    where
        A: BaseTypeWitness,
        B: BaseTypeWitness,
        A::L: Sized,
        A::R: Sized,
    {
        SomeTypeArgIsNe::A(TypeEq::NEW).zip3(self, other1, other2)
    }

    /// Combines this `TypeNe<L, R>` with 
    /// three [`BaseTypeWitness`](BaseTypeWitness)es to produce a
    /// `TypeNe<(L, A::L, B::L, C::L), (R, A::R, B::R, C::R)> `.
    pub const fn zip4<A, B, C>(
        self: TypeNe<L, R>,
        other1: A,
        other2: B,
        other3: C,
    ) -> TypeNe<(L, A::L, B::L, C::L), (R, A::R, B::R, C::R)> 
    where
        A: BaseTypeWitness,
        B: BaseTypeWitness,
        C: BaseTypeWitness,
        A::L: Sized,
        A::R: Sized,
        B::L: Sized,
        B::R: Sized,
    {
        SomeTypeArgIsNe::A(TypeEq::NEW).zip4(self, other1, other2, other3)
    }
}



// using this instead of `mod extra_type_ne_methods;`
// to document the impls in the submodule below the constructors.
#[cfg(feature = "inj_type_fn")]
include!{"./type_ne/extra_type_ne_methods.rs"}



impl<L: ?Sized, R: ?Sized> Copy for TypeNe<L, R> {}

impl<L: ?Sized, R: ?Sized> Clone for TypeNe<L, R> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<L: ?Sized, R: ?Sized> Debug for TypeNe<L, R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TypeNe")
    }
}

impl<L: ?Sized, R: ?Sized> PartialEq for TypeNe<L, R> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<L: ?Sized, R: ?Sized> PartialOrd for TypeNe<L, R> {
    fn partial_cmp(&self, _: &Self) -> Option<Ordering> {
        Some(Ordering::Equal)
    }
}

impl<L: ?Sized, R: ?Sized> Ord for TypeNe<L, R> {
    fn cmp(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}

impl<L: ?Sized, R: ?Sized> Eq for TypeNe<L, R> {}

impl<L: ?Sized, R: ?Sized> Hash for TypeNe<L, R> {
    fn hash<H>(&self, _state: &mut H)
    where H: Hasher
    {}
}



