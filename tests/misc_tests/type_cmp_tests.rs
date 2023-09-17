
use typewit::{TypeCmp::{self, TEq, TNe}, TypeEq, TypeNe};

use crate::misc_tests::test_utils::assert_type;


#[cfg(feature = "inj_type_fn")]
mod typecmp_extra_method_tests;


#[test]
fn test_with_any() {
    {
        let x = TypeCmp::<u8, u8>::with_any();
        assert_type::<_, TypeCmp<u8, u8>>(x);
        assert!(matches!(x, TEq(_)));
    }
    {
        let x = TypeCmp::<u8, i8>::with_any();
        assert_type::<_, TypeCmp<u8, i8>>(x);
        assert!(matches!(x, TNe(_)));
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
fn test_teq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.teq();
        cmp
    }

    {
        let x = const_callable(TypeCmp::<u8, i8>::with_any()).teq();
        assert_type::<_, Option<TypeEq<u8, i8>>>(x);
        assert!(matches!(x, None{}));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::with_any()).teq();
        assert_type::<_, Option<TypeEq<u8, u8>>>(x);
        assert!(matches!(x, Some(_)));
    }
}

#[test]
fn test_tne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.tne();
        cmp
    }

    {
        let x = const_callable(TypeCmp::<u8, i8>::with_any()).tne();
        assert_type::<_, Option<TypeNe<u8, i8>>>(x);
        assert!(matches!(x, Some(_)));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::with_any()).tne();
        assert_type::<_, Option<TypeNe<u8, u8>>>(x);
        assert!(matches!(x, None{}));
    }
}

#[test]
fn test_is_teq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.is_teq();
        cmp
    }

    assert!( const_callable(TypeCmp::<u8, u8>::with_any()).is_teq());
    assert!(!const_callable(TypeCmp::<u8, i8>::with_any()).is_teq());
}

#[test]
fn test_is_tne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.is_tne();
        cmp
    }

    assert!(!const_callable(TypeCmp::<u8, u8>::with_any()).is_tne());
    assert!( const_callable(TypeCmp::<u8, i8>::with_any()).is_tne());
}

#[test]
fn test_unwrap_teq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_teq();
        cmp
    }

    let x = const_callable(TypeCmp::<u8, u8>::with_any()).unwrap_teq();
    assert_type::<_, TypeEq<u8, u8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_teq_panicking() {
    TypeCmp::<u8, i8>::with_any().unwrap_teq();
}


#[test]
fn test_unwrap_tne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_tne();
        cmp
    }

    let x = const_callable(TypeCmp::<u8, i8>::with_any()).unwrap_tne();
    assert_type::<_, TypeNe<u8, i8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_tne_panicking() {
    TypeCmp::<u8, u8>::with_any().unwrap_tne();
}
