use typewit::{TypeEq, TypeNe};
use typewit::type_fn::{GRef, GRefMut, TypeFn};

use crate::misc_tests::test_utils::{assert_not_type, assert_type, assert_type_ne};

use std::mem::{align_of, size_of};

const fn typene<L, R>() -> TypeNe<L, R> {
    assert!(size_of::<L>() != size_of::<R>() || align_of::<L>() != align_of::<R>());

    // SAFETY: if either the size or alignment doesn't match,
    //         then L and R are different types.
    unsafe {
        TypeNe::new_unchecked()
    }
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
fn map_test() {
    assert_type::<_, TypeNe<&u8, &u16>>(typene::<u8, u16>().map(GRef::NEW));
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(typene::<u8, u16>().map(GRefMut::NEW));
}

#[test]
fn project_test() {
    #[derive(Debug, PartialEq)]
    struct Foo<T>(T);

    struct FooFn;
    impl<T> TypeFn<T> for FooFn {
        type Output = Foo<T>;
    }
    impl<T> typewit::RevTypeFn<Foo<T>> for FooFn {
        type Arg = T;
    }


    assert_type::<_, TypeNe<&u8, &u16>>(typene::<u8, u16>().project::<GRef<'_>>());
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(typene::<u8, u16>().project::<GRefMut<'_>>());
    assert_type::<_, TypeNe<Foo<u8>, Foo<u16>>>(typene::<u8, u16>().project::<FooFn>());
}



#[test]
fn zip_test() {
    const fn do_zip<A, B>(
        left: TypeNe<A, u8>,
        right: TypeNe<B, &'static str>,
    ) -> TypeNe<(A, B), (u8, &'static str)> {
        left.zip(right)
    }

    let _ = do_zip(typene::<u16, _>(), typene::<u16, _>());
}

#[test]
fn zip3_test() {
    const fn do_zip<A, B, C>(
        a: TypeNe<A, u8>,
        b: TypeNe<B, &'static str>,
        c: TypeNe<C, Vec<u8>>,
    ) -> TypeNe<(A, B, C), (u8, &'static str, Vec<u8>)> {
        a.zip3(b, c)
    }

    let _ = do_zip(typene::<u16, _>(), typene::<u16, _>(), typene::<u16, _>());
}

#[test]
fn zip4_test() {
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