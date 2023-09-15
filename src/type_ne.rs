use core::{
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
    fmt::{self, Debug},
};

use crate::TypeEq;

pub use self::type_ne_::TypeNe;

mod type_ne_ {
    use core::marker::PhantomData;

    /// Value-level proof that `L` is a different type to `R`
    /// 
    /// The opposite of [`TypeEq`](crate::TypeEq).
    /// 
    /// Because there is (as of 2023-05-14)
    /// no way to generically check that types are unequal, 
    /// this type is of limited usefulness.
    /// 
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
    /// Swaps the type arguments of this `TypeNe`
    pub const fn flip(self: TypeNe<L, R>) -> TypeNe<R, L> {
        // SAFETY: type inequality is commutative
        unsafe { TypeNe::<R, L>::new_unchecked() }
    }

    /// Joins a proof of `L != R` with a proof of `J == L`,
    /// creating a proof of `J != R.`
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


crate::type_eq_ne_guts::declare_zip_helper!{
    $ TypeNe
}


impl<L0, R0> TypeNe<L0, R0> {
    /// Combines two `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1), (R0, R1)>`.
    pub const fn zip<L1, R1>(
        self: TypeNe<L0, R0>,
        other: TypeNe<L1, R1>,
    ) -> TypeNe<(L0, L1), (R0, R1)> {
        zip_impl!{self[L0, R0], other[L1, R1]}
    }

    /// Combines three `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1, L2), (R0, R1, R2)>`.
    pub const fn zip3<L1, R1, L2, R2>(
        self: TypeNe<L0, R0>,
        other1: TypeNe<L1, R1>,
        other2: TypeNe<L2, R2>,
    ) -> TypeNe<(L0, L1, L2), (R0, R1, R2)> {
        zip_impl!{
            self[L0, R0],
            other1[L1, R1],
            other2[L2, R2],
        }
    }

    /// Combines four `TypeNe<L*, R*>` to produce a
    /// `TypeNe<(L0, L1, L2, L3), (R0, R1, R2, R3)>`.
    pub const fn zip4<L1, R1, L2, R2, L3, R3>(
        self: TypeNe<L0, R0>,
        other1: TypeNe<L1, R1>,
        other2: TypeNe<L2, R2>,
        other3: TypeNe<L3, R3>,
    ) -> TypeNe<(L0, L1, L2, L3), (R0, R1, R2, R3)> {
        zip_impl!{
            self[L0, R0],
            other1[L1, R1],
            other2[L2, R2],
            other3[L3, R3],
        }
    }
}



#[cfg(feature = "inj_type_fn")]
mod extra_type_ne_methods;



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



