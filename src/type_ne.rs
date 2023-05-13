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