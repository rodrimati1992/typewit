use typewit::const_marker::{
    Bool, Char,
    U8, U16, U32, U64, U128, Usize, I8, I16, I32, I64, I128, Isize,
};


#[test]
fn test_integer_const_marker() {

    macro_rules! shared_test_case {
        ($ty:ident) => ({
            match $ty::<0>.eq($ty::<1>) {
                Ok(typewit::TypeEq::<$ty<0>, $ty<1>>{..}) => panic!("OH NO"),
                Err(typewit::TypeNe::<$ty<0>, $ty<1>>{..}) => (),
            }

            match $ty::<1>.eq($ty::<1>) {
                Ok(typewit::TypeEq::<$ty<1>, $ty<1>>{..}) => (),
                Err(typewit::TypeNe::<$ty<1>, $ty<1>>{..}) => panic!("OH NO"),
            }
        });
    }

    shared_test_case!{U8}
    shared_test_case!{U16}
    shared_test_case!{U32}
    shared_test_case!{U64}
    shared_test_case!{U128}
    shared_test_case!{Usize}

    shared_test_case!{I8}
    shared_test_case!{I16}
    shared_test_case!{I32}
    shared_test_case!{I64}
    shared_test_case!{I128}
    shared_test_case!{Isize}
}

#[test]
fn test_char_const_marker() {
    match Char::<'a'>.eq(Char::<'4'>) {
        Ok(typewit::TypeEq::<Char<'a'>, Char<'4'>>{..}) => panic!("OH NO"),
        Err(typewit::TypeNe::<Char<'a'>, Char<'4'>>{..}) => (),
    }

    match Char::<'4'>.eq(Char::<'4'>) {
        Ok(typewit::TypeEq::<Char<'4'>, Char<'4'>>{..}) => (),
        Err(typewit::TypeNe::<Char<'4'>, Char<'4'>>{..}) => panic!("OH NO"),
    }
}

#[test]
fn test_bool_const_marker() {
    match Bool::<false>.eq(Bool::<true>) {
        Ok(typewit::TypeEq::<Bool<false>, Bool<true>>{..}) => panic!("OH NO"),
        Err(typewit::TypeNe::<Bool<false>, Bool<true>>{..}) => (),
    }

    match Bool::<true>.eq(Bool::<false>) {
        Ok(typewit::TypeEq::<Bool<true>, Bool<false>>{..}) => panic!("OH NO"),
        Err(typewit::TypeNe::<Bool<true>, Bool<false>>{..}) => (),
    }

    match Bool::<false>.eq(Bool::<false>) {
        Ok(typewit::TypeEq::<Bool<false>, Bool<false>>{..}) => (),
        Err(typewit::TypeNe::<Bool<false>, Bool<false>>{..}) => panic!("OH NO"),
    }

    match Bool::<true>.eq(Bool::<true>) {
        Ok(typewit::TypeEq::<Bool<true>, Bool<true>>{..}) => (),
        Err(typewit::TypeNe::<Bool<true>, Bool<true>>{..}) => panic!("OH NO"),
    }
}
