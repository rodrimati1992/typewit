typewit::inj_type_fn!{
    struct ToU16;

    impl u8  => u16;
    impl u16 => u16;
}

typewit::inj_type_fn!{
    struct IntoGeneric;

    impl<T> Vec<T> => T;
    impl<T> [T; 0] => T;
}

typewit::inj_type_fn!{
    struct IntoGenericAndConcrete;

    impl u8 => u8;
    impl<T> [T; 0] => T;
}

fn main(){}