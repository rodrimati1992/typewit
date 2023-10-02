use typewit::{TypeCmp, TypeEq, TypeNe};

use crate::misc_tests::test_utils::{assert_type, assert_type_ne};

use std::mem::{align_of, size_of};

mod typene_extra_method_tests;


#[track_caller]
fn typene<L: 'static, R: 'static>() -> TypeNe<L, R> {
    TypeNe::with_any().unwrap()
}


#[test]
fn with_any_test() {
    assert!(TypeNe::<u32, u32>::with_any().is_none());
    assert!(TypeNe::<i32, i32>::with_any().is_none());
    
    let _: TypeNe<u32, i32> = TypeNe::<u32, i32>::with_any().unwrap();
}

#[test]
fn flip_method() {
    fn flipper<L, R>(te: TypeNe<L, R>) {
        assert_type_ne(te.flip(), te);
        let _ = |te: TypeNe<u8, u16>| -> TypeNe<u16, u8> { te.flip() };
    }
    flipper(typene::<u8, u16>());
}

#[test]
fn join_left_method() {
    fn joiner<A, B, Q>(tea: TypeNe<A, B>, teb: TypeEq<Q, A>) {
        let _: TypeNe<Q, B> = tea.join_left(teb);
        assert_type::<_, TypeNe<Q, B>>(tea.join_left(teb));
    }
    joiner(typene::<u8, u16>(), TypeEq::NEW);
}

#[test]
fn join_right_method() {
    fn joiner<A, B, Q>(tea: TypeNe<A, B>, teb: TypeEq<B, Q>) {
        let _: TypeNe<A, Q> = tea.join_right(teb);
        assert_type::<_, TypeNe<A, Q>>(tea.join_right(teb));
    }
    joiner(typene::<u8, u16>(), TypeEq::NEW);
}


#[test]
#[cfg(feature = "generic_fns")]
fn zip_test() {
    {
        const fn do_zip<A, B>(
            left: TypeNe<A, u8>,
            right: TypeNe<B, &'static str>,
        ) -> TypeNe<(A, B), (u8, &'static str)> {
            left.zip(right)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>());
    }

    {
        const fn do_zip<A, B>(
            left: TypeNe<A, u8>,
            right: TypeEq<B, &'static str>,
        ) -> TypeNe<(A, B), (u8, &'static str)> {
            left.zip(right)
        }

        let _ = do_zip(typene::<u16, _>(), TypeEq::NEW);        
    }

    {
        const fn do_zip<A, B>(
            left: TypeNe<A, u8>,
            right: TypeCmp<B, &'static str>,
        ) -> TypeNe<(A, B), (u8, &'static str)> {
            left.zip(right)
        }

        let _ = do_zip(typene::<u16, _>(), TypeCmp::Ne(typene::<u16, _>()));
        let _ = do_zip(typene::<u16, _>(), TypeCmp::Eq(TypeEq::NEW));
    }
}

#[test]
#[cfg(feature = "generic_fns")]
fn zip3_test() {
    {
        const fn do_zip<A, B, C>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
        ) -> TypeNe<(A, B, C), (u8, &'static str, Vec<u8>)> {
            a.zip3(b, c)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), typene::<u16, _>());
    }

    {
        const fn do_zip<A, B, C>(
            a: TypeNe<A, u8>,
            b: TypeEq<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
        ) -> TypeNe<(A, B, C), (u8, &'static str, Vec<u8>)> {
            a.zip3(b, c)
        }

        let _ = do_zip(typene::<u16, _>(), TypeEq::NEW, typene::<u16, _>());
    }

    {
        const fn do_zip<A, B, C>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeEq<C, Vec<u8>>,
        ) -> TypeNe<(A, B, C), (u8, &'static str, Vec<u8>)> {
            a.zip3(b, c)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), TypeEq::NEW);
    }

    {
        const fn do_zip<A, B, C>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeCmp<C, Vec<u8>>,
        ) -> TypeNe<(A, B, C), (u8, &'static str, Vec<u8>)> {
            a.zip3(b, c)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), TypeCmp::Ne(typene::<u16, _>()));
        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), TypeCmp::Eq(TypeEq::NEW));
    }
}

#[test]
#[cfg(feature = "generic_fns")]
fn zip4_test() {
    {
        const fn do_zip<A, B, C, D>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
            d: TypeNe<D, [u8; 2]>,
        ) -> TypeNe<(A, B, C, D), (u8, &'static str, Vec<u8>, [u8; 2])> {
            a.zip4(b, c, d)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), typene::<u16, _>(), typene::<u16, _>());
    }

    {
        const fn do_zip<A, B, C, D>(
            a: TypeNe<A, u8>,
            b: TypeEq<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
            d: TypeNe<D, [u8; 2]>,
        ) -> TypeNe<(A, B, C, D), (u8, &'static str, Vec<u8>, [u8; 2])> {
            a.zip4(b, c, d)
        }

        let _ = do_zip(typene::<u16, _>(), TypeEq::NEW, typene::<u16, _>(), typene::<u16, _>());
    }

    {
        const fn do_zip<A, B, C, D>(
            a: TypeNe<A, u8>,
            b: TypeCmp<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
            d: TypeNe<D, [u8; 2]>,
        ) -> TypeNe<(A, B, C, D), (u8, &'static str, Vec<u8>, [u8; 2])> {
            a.zip4(b, c, d)
        }

        let _ = do_zip(
            typene::<u16, _>(),
            TypeCmp::Eq(TypeEq::NEW),
            typene::<u16, _>(),
            typene::<u16, _>(),
        );

        let _ = do_zip(
            typene::<u16, _>(),
            TypeCmp::Ne(typene::<u16, _>()),
            typene::<u16, _>(),
            typene::<u16, _>(),
        );
    }

    {
        const fn do_zip<A, B, C, D>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeEq<C, Vec<u8>>,
            d: TypeNe<D, [u8; 2]>,
        ) -> TypeNe<(A, B, C, D), (u8, &'static str, Vec<u8>, [u8; 2])> {
            a.zip4(b, c, d)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), TypeEq::NEW, typene::<u16, _>());
    }

    {
        const fn do_zip<A, B, C, D>(
            a: TypeNe<A, u8>,
            b: TypeNe<B, &'static str>,
            c: TypeNe<C, Vec<u8>>,
            d: TypeEq<D, [u8; 2]>,
        ) -> TypeNe<(A, B, C, D), (u8, &'static str, Vec<u8>, [u8; 2])> {
            a.zip4(b, c, d)
        }

        let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), typene::<u16, _>(), TypeEq::NEW);
    }
}


#[test]
fn type_ne_macro_test() {
    const fn constness<T, U: ?Sized, const N: usize>() {
        let _: TypeNe<u8, u16> = typewit::type_ne!{u8, u16};

        let _: TypeNe<u8, u16> = typewit::type_ne!{<> u8, u16};
        
        let _: TypeNe<str, (str,)> = typewit::type_ne!{<X: ?Sized> X, (X,)};

        let _: TypeNe<T, [T; N]> = typewit::type_ne!{<X, const N: usize> X, [X; N]};

        let _: TypeNe<u32, [u32]> = typewit::type_ne!{<X: Copy> X, [X] };

        let _: TypeNe<u32, [u32]> = typewit::type_ne!{<X> X, [X] where X: Copy};

    }
    constness::<u64, str, 10>();
}








