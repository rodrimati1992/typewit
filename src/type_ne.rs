use core::{
    cmp::{Ordering, Eq, Ord, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
    fmt::{self, Debug},
};


#[cfg(feature = "inj_type_fn")]
mod extra_type_ne_methods;


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



