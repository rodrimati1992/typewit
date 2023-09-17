use typewit::{
    const_marker::{
        Bool, Char,
        U8, U16, U32, U64, U128, Usize, I8, I16, I32, I64, I128, Isize,
    },
    TypeCmp,
};


#[cfg(feature = "adt_const_marker")]
mod slice_const_marker_tests;


#[test]
fn test_integer_const_marker() {

    macro_rules! shared_test_case {
        ($ty:ident, $prim:ty) => ({
            const N: $prim = {
                let n: $prim = 0;
                n.wrapping_sub(1)
            };
            const Z: $prim = 0;
            const P: $prim = 1;

            match $ty::<N>.equals($ty::<Z>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<N>, $ty<Z>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<N>, $ty<Z>>{..}) => (),
            }

            match $ty::<Z>.equals($ty::<N>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<Z>, $ty<N>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<Z>, $ty<N>>{..}) => (),
            }

            match $ty::<P>.equals($ty::<N>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<P>, $ty<N>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<P>, $ty<N>>{..}) => (),
            }

            match $ty::<N>.equals($ty::<P>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<N>, $ty<P>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<N>, $ty<P>>{..}) => (),
            }

            match $ty::<P>.equals($ty::<Z>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<P>, $ty<Z>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<P>, $ty<Z>>{..}) => (),
            }

            match $ty::<Z>.equals($ty::<P>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<Z>, $ty<P>>{..}) => panic!("OH NO"),
                TypeCmp::Ne(typewit::TypeNe::<$ty<Z>, $ty<P>>{..}) => (),
            }

            match $ty::<N>.equals($ty::<N>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<N>, $ty<N>>{..}) => (),
                TypeCmp::Ne(typewit::TypeNe::<$ty<N>, $ty<N>>{..}) => panic!("OH NO"),
            }

            match $ty::<Z>.equals($ty::<Z>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<Z>, $ty<Z>>{..}) => (),
                TypeCmp::Ne(typewit::TypeNe::<$ty<Z>, $ty<Z>>{..}) => panic!("OH NO"),
            }

            match $ty::<P>.equals($ty::<P>) {
                TypeCmp::Eq(typewit::TypeEq::<$ty<P>, $ty<P>>{..}) => (),
                TypeCmp::Ne(typewit::TypeNe::<$ty<P>, $ty<P>>{..}) => panic!("OH NO"),
            }
        });
    }

    shared_test_case!{U8, u8}
    shared_test_case!{U16, u16}
    shared_test_case!{U32, u32}
    shared_test_case!{U64, u64}
    shared_test_case!{U128, u128}
    shared_test_case!{Usize, usize}

    shared_test_case!{I8, i8}
    shared_test_case!{I16, i16}
    shared_test_case!{I32, i32}
    shared_test_case!{I64, i64}
    shared_test_case!{I128, i128}
    shared_test_case!{Isize, isize}
}

#[test]
fn test_char_const_marker() {
    match Char::<'a'>.equals(Char::<'4'>) {
        TypeCmp::Eq(typewit::TypeEq::<Char<'a'>, Char<'4'>>{..}) => panic!("OH NO"),
        TypeCmp::Ne(typewit::TypeNe::<Char<'a'>, Char<'4'>>{..}) => (),
    }

    match Char::<'4'>.equals(Char::<'4'>) {
        TypeCmp::Eq(typewit::TypeEq::<Char<'4'>, Char<'4'>>{..}) => (),
        TypeCmp::Ne(typewit::TypeNe::<Char<'4'>, Char<'4'>>{..}) => panic!("OH NO"),
    }
}

#[test]
fn test_bool_const_marker() {
    match Bool::<false>.equals(Bool::<true>) {
        TypeCmp::Eq(typewit::TypeEq::<Bool<false>, Bool<true>>{..}) => panic!("OH NO"),
        TypeCmp::Ne(typewit::TypeNe::<Bool<false>, Bool<true>>{..}) => (),
    }

    match Bool::<true>.equals(Bool::<false>) {
        TypeCmp::Eq(typewit::TypeEq::<Bool<true>, Bool<false>>{..}) => panic!("OH NO"),
        TypeCmp::Ne(typewit::TypeNe::<Bool<true>, Bool<false>>{..}) => (),
    }

    match Bool::<false>.equals(Bool::<false>) {
        TypeCmp::Eq(typewit::TypeEq::<Bool<false>, Bool<false>>{..}) => (),
        TypeCmp::Ne(typewit::TypeNe::<Bool<false>, Bool<false>>{..}) => panic!("OH NO"),
    }

    match Bool::<true>.equals(Bool::<true>) {
        TypeCmp::Eq(typewit::TypeEq::<Bool<true>, Bool<true>>{..}) => (),
        TypeCmp::Ne(typewit::TypeNe::<Bool<true>, Bool<true>>{..}) => panic!("OH NO"),
    }
}
