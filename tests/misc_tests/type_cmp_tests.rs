
use typewit::{TypeCmp, TypeEq, TypeNe};

use crate::misc_tests::test_utils::assert_type;


#[cfg(feature = "inj_type_fn")]
mod typecmp_extra_method_tests;


#[test]
fn test_with_any() {
    {
        let x = TypeCmp::<u8, u8>::with_any();
        assert_type::<_, TypeCmp<u8, u8>>(x);
        assert!(matches!(x, TypeCmp::Eq(_)));
    }
    {
        let x = TypeCmp::<u8, i8>::with_any();
        assert_type::<_, TypeCmp<u8, i8>>(x);
        assert!(matches!(x, TypeCmp::Ne(_)));
    }
}

#[test]
fn test_flip() {
    let x = TypeCmp::<u8, i8>::with_any().flip();
    assert_type::<_, TypeCmp<i8, u8>>(x);

    const fn _const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) {
        let _: TypeCmp<U, T> = cmp.flip();
    }
}

#[test]
fn test_join_left() {
    const fn const_callable<O: ?Sized, T: ?Sized, U: ?Sized>(
        left: TypeEq<O, T>,
        cmp: TypeCmp<T, U>,
    ) -> TypeCmp<O, U> {
        cmp.join_left(left)
    }

    {
        let x = const_callable::<u8, u8, u8>(TypeEq::NEW, TypeCmp::with_any());
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = const_callable::<u8, u8, i8>(TypeEq::NEW, TypeCmp::with_any());
        assert_type::<_, TypeCmp<u8, i8>>(x);
    }
}

#[test]
fn test_join_right() {
    const fn const_callable<T: ?Sized, U: ?Sized, R: ?Sized>(
        cmp: TypeCmp<T, U>,
        right: TypeEq<U, R>,
    ) -> TypeCmp<T, R> {
        cmp.join_right(right)
    }

    {
        let x = const_callable::<u8, u8, u8>(TypeCmp::with_any(), TypeEq::NEW);
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = const_callable::<u8, i8, i8>(TypeCmp::with_any(), TypeEq::NEW);
        assert_type::<_, TypeCmp<u8, i8>>(x);
    }
}

#[test]
fn test_eq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.eq();
        cmp
    }

    {
        let x = const_callable(TypeCmp::<u8, i8>::with_any()).eq();
        assert_type::<_, Option<TypeEq<u8, i8>>>(x);
        assert!(matches!(x, None{}));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::with_any()).eq();
        assert_type::<_, Option<TypeEq<u8, u8>>>(x);
        assert!(matches!(x, Some(_)));
    }
}

#[test]
fn test_ne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.ne();
        cmp
    }

    {
        let x = const_callable(TypeCmp::<u8, i8>::with_any()).ne();
        assert_type::<_, Option<TypeNe<u8, i8>>>(x);
        assert!(matches!(x, Some(_)));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::with_any()).ne();
        assert_type::<_, Option<TypeNe<u8, u8>>>(x);
        assert!(matches!(x, None{}));
    }
}

#[test]
fn test_is_eq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.is_eq();
        cmp
    }

    assert!( const_callable(TypeCmp::<u8, u8>::with_any()).is_eq());
    assert!(!const_callable(TypeCmp::<u8, i8>::with_any()).is_eq());
}

#[test]
fn test_is_ne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.is_ne();
        cmp
    }

    assert!(!const_callable(TypeCmp::<u8, u8>::with_any()).is_ne());
    assert!( const_callable(TypeCmp::<u8, i8>::with_any()).is_ne());
}

#[test]
fn test_unwrap_eq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_eq();
        cmp
    }

    let x = const_callable(TypeCmp::<u8, u8>::with_any()).unwrap_eq();
    assert_type::<_, TypeEq<u8, u8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_eq_panicking() {
    TypeCmp::<u8, i8>::with_any().unwrap_eq();
}


#[test]
fn test_unwrap_ne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_ne();
        cmp
    }

    let x = const_callable(TypeCmp::<u8, i8>::with_any()).unwrap_ne();
    assert_type::<_, TypeNe<u8, i8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_ne_panicking() {
    TypeCmp::<u8, u8>::with_any().unwrap_ne();
}

#[cfg(feature = "rust_1_61")]
#[test]
fn test_zip_method() {
    const fn constness<A, B, C, D, E, F>(
        eq: TypeEq<A, B>,
        ne: TypeNe<B, C>, 
        cmp_eq: TypeCmp<D, E>,
        cmp_ne: TypeCmp<E, F>,
    ) {
        {
            let x: TypeCmp<(D, A), (E, B)> = cmp_eq.zip(eq);
            assert!(matches!(x, TypeCmp::Eq{..}));
        }
        {
            let x: TypeCmp<(D, B), (E, C)> = cmp_eq.zip(ne);
            assert!(matches!(x, TypeCmp::Ne{..}));
        }
        {
            let x: TypeCmp<(D, E), (E, D)> = cmp_eq.zip(cmp_eq.flip());
            assert!(matches!(x, TypeCmp::Eq{..}));
        }
        {
            let x: TypeCmp<(D, E), (E, F)> = cmp_eq.zip(cmp_ne);
            assert!(matches!(x, TypeCmp::Ne{..}));
        }
    }

    constness::<u8, u8, bool, u32, u32, u64>(
        TypeEq::NEW,
        TypeNe::with_any().unwrap(),
        TypeCmp::with_any(),
        TypeCmp::with_any(),
    );

}


