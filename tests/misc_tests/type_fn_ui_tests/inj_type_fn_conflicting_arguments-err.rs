typewit::inj_type_fn!{
    struct Foo;

    impl u8  => u16;
    impl<T> T => Vec<T>;
}


fn main(){}