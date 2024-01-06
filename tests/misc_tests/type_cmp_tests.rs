
use typewit::{TypeCmp, TypeEq, TypeNe, type_ne};

use crate::misc_tests::test_utils::{assertm, assert_type};


mod typecmp_extra_method_tests;


#[allow(deprecated)]
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
    let x = TypeCmp::Ne(type_ne!(u8, i8)).flip();
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
        let x = const_callable::<u8, u8, u8>(TypeEq::NEW, TypeCmp::Eq(TypeEq::NEW));
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = const_callable::<u8, u8, i8>(TypeEq::NEW, TypeCmp::Ne(type_ne!(u8, i8)));
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
        let x = const_callable::<u8, u8, u8>(TypeCmp::Eq(TypeEq::NEW), TypeEq::NEW);
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = const_callable::<u8, i8, i8>(TypeCmp::Ne(type_ne!(u8, i8)), TypeEq::NEW);
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
        let x = const_callable(TypeCmp::Ne(type_ne!(u8, i8))).eq();
        assert_type::<_, Option<TypeEq<u8, i8>>>(x);
        assert!(matches!(x, None{}));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::Eq(TypeEq::NEW)).eq();
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
        let x = const_callable(TypeCmp::Ne(type_ne!(u8, i8))).ne();
        assert_type::<_, Option<TypeNe<u8, i8>>>(x);
        assert!(matches!(x, Some(_)));
    }
    {
        let x = const_callable(TypeCmp::<u8, u8>::Eq(TypeEq::NEW)).ne();
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

    assert!( const_callable(TypeCmp::<u8, u8>::Eq(TypeEq::NEW)).is_eq());
    assert!(!const_callable(TypeCmp::Ne(type_ne!(u8, i8))).is_eq());
}

#[test]
fn test_is_ne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.is_ne();
        cmp
    }

    assert!(!const_callable(TypeCmp::<u8, u8>::Eq(TypeEq::NEW)).is_ne());
    assert!( const_callable(TypeCmp::Ne(type_ne!(u8, i8))).is_ne());
}

#[test]
fn test_unwrap_eq() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_eq();
        cmp
    }

    let x = const_callable(TypeCmp::<u8, u8>::Eq(TypeEq::NEW)).unwrap_eq();
    assert_type::<_, TypeEq<u8, u8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_eq_panicking() {
    TypeCmp::Ne(type_ne!(u8, i8)).unwrap_eq();
}


#[test]
fn test_unwrap_ne() {
    const fn const_callable<T: ?Sized, U: ?Sized>(cmp: TypeCmp<T, U>) -> TypeCmp<T, U> {
        let _ = cmp.unwrap_ne();
        cmp
    }

    let x = const_callable(TypeCmp::Ne(type_ne!(u8, i8))).unwrap_ne();
    assert_type::<_, TypeNe<u8, i8>>(x);
}

#[test]
#[should_panic]
fn test_unwrap_ne_panicking() {
    TypeCmp::<u8, u8>::Eq(TypeEq::NEW).unwrap_ne();
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
        type_ne!(u8, bool),
        TypeCmp::Eq(TypeEq::NEW),
        TypeCmp::Ne(type_ne!(u32, u64)),
    );

}



#[cfg(feature = "rust_1_61")]
#[test]
fn test_zip3_method() {
    const fn constness<A, B, C, D, E, F, G>(
        eq0: TypeEq<A, B>,
        eq1: TypeEq<B, C>,
        ne: TypeNe<C, D>, 
        cmp_eq: TypeCmp<E, F>,
        cmp_ne: TypeCmp<F, G>,
    ) {
        {
            let x = cmp_eq.zip3(eq0, eq1);
            assertm!(x, TypeCmp::<(E, A, B), (F, B, C)>::Eq(_));
        }
        {
            let x = cmp_eq.zip3(eq0, ne);
            assertm!(x, TypeCmp::<(E, A, C), (F, B, D)>::Ne(_));
        }
        {
            let x = cmp_eq.zip3(eq0, cmp_ne);
            assertm!(x, TypeCmp::<(E, A, F), (F, B, G)>::Ne(_));
        }
        {
            let x = cmp_eq.zip3(ne, eq0);
            assertm!(x, TypeCmp::<(E, C, A), (F, D, B)>::Ne(_));
        }
        {
            let x = cmp_eq.zip3(cmp_ne, eq0);
            assertm!(x, TypeCmp::<(E, F, A), (F, G, B)>::Ne(_));
        }

        {
            let x = cmp_ne.zip3(eq0, eq1);
            assertm!(x, TypeCmp::<(F, A, B), (G, B, C)>::Ne(_));
        }
    }

    constness::<u8, u8, u8, bool, u32, u32, u64>(
        TypeEq::NEW,
        TypeEq::NEW,
        type_ne!(u8, bool),
        TypeCmp::Eq(TypeEq::NEW),
        TypeCmp::Ne(type_ne!(u32, u64)),
    );
}

#[cfg(feature = "rust_1_61")]
#[test]
fn test_zip4_method() {
    const fn with<A, B, C, D, E, F, G>(
        eq: TypeEq<A, B>, 
        ne: TypeNe<B, C>, 
        cmp_eq: TypeCmp<D, E>,
        cmp_ne: TypeCmp<F, G>,
    ) {
        assertm!(
            cmp_ne.zip4(eq, eq.flip(), ne),
            TypeCmp::<(F, A, B, B), (G, B, A, C)>::Ne{..}
        );
        assertm!(
            cmp_ne.zip4(eq, eq.flip(), cmp_ne.flip()),
            TypeCmp::<(F, A, B, G), (G, B, A, F)>::Ne{..}
        );
        assertm!(
            cmp_ne.zip4(cmp_eq, eq.flip(), eq),
            TypeCmp::<(F, D, B, A), (G, E, A, B)>::Ne{..}
        );
        assertm!(
            cmp_eq.zip4(eq, eq.flip(), cmp_eq),
            TypeCmp::<(D, A, B, D), (E, B, A, E)>::Eq{..},
        );
        assertm!(
            cmp_eq.zip4(cmp_eq.flip(), eq.flip(), eq),
            TypeCmp::<(D, E, B, A), (E, D, A, B)>::Eq{..},
        );
    }

    with::<u8, u8, bool, u16, u16, u32, u64>(
        TypeEq::NEW, 
        type_ne!(u8, bool), 
        TypeCmp::Eq(TypeEq::NEW),
        TypeCmp::Ne(type_ne!(u32, u64)),
    );
}

#[cfg(feature = "rust_1_61")]
#[test]
fn test_in_array_method() {
    use typewit::{
        const_marker::Usize,
        TypeCmp, TypeEq, TypeNe,
    };
    
    let cmp_eq_ty: TypeCmp<i32, i32> = TypeCmp::Eq(TypeEq::NEW);
    let cmp_ne_ty: TypeCmp<i64, u64> = TypeCmp::Ne(type_ne!(i64, u64));
    
    let eq_len: TypeEq<Usize<0>, Usize<0>> = TypeEq::NEW;
    let ne_len: TypeNe<Usize<1>, Usize<2>> = Usize.equals(Usize).unwrap_ne();
    let cmp_eq_len: TypeCmp<Usize<3>, Usize<3>> = Usize.equals(Usize);
    let cmp_ne_len: TypeCmp<Usize<5>, Usize<8>> = Usize.equals(Usize);

    with(
        cmp_eq_ty,
        cmp_ne_ty,
        eq_len,
        ne_len,
        cmp_eq_len,
        cmp_ne_len,
    );

    const fn with<
        C, D, E,
        const J: usize,
        const K: usize,
        const L: usize,
        const M: usize,
        const N: usize,
        const O: usize,
    >(
        cmp_eq_ty: TypeCmp<C, C>,
        cmp_ne_ty: TypeCmp<D, E>,
        eq_len: TypeEq<Usize<J>, Usize<J>>,
        ne_len: TypeNe<Usize<K>, Usize<L>>,
        cmp_eq_len: TypeCmp<Usize<M>, Usize<M>>,
        cmp_ne_len: TypeCmp<Usize<N>, Usize<O>>,
    ) {
        assertm!(cmp_eq_ty.in_array(eq_len), TypeCmp::<[C; J], [C; J]>::Eq(_));
        assertm!(cmp_eq_ty.in_array(ne_len), TypeCmp::<[C; K], [C; L]>::Ne(_));
        assertm!(cmp_eq_ty.in_array(cmp_eq_len), TypeCmp::<[C; M], [C; M]>::Eq(_));
        assertm!(cmp_eq_ty.in_array(cmp_ne_len), TypeCmp::<[C; N], [C; O]>::Ne(_));
        
        assertm!(cmp_ne_ty.in_array(eq_len), TypeCmp::<[D; J], [E; J]>::Ne(_));
        assertm!(cmp_ne_ty.in_array(ne_len), TypeCmp::<[D; K], [E; L]>::Ne(_));
        assertm!(cmp_ne_ty.in_array(cmp_eq_len), TypeCmp::<[D; M], [E; M]>::Ne(_));
        assertm!(cmp_ne_ty.in_array(cmp_ne_len), TypeCmp::<[D; N], [E; O]>::Ne(_));
    }
}
