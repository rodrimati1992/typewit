typewit::type_fn!{
    struct Foo where u8:;

    () => ()
}

typewit::type_fn!{
    struct Bar;

    () => ()
    where u8:;
}

typewit::type_fn!{
    struct Baz;

    for[T] T => T
    where T:;
}

typewit::type_fn!{
    struct WithTypeParam<T>;
    () => ()
}

typewit::type_fn!{
    struct NakedForBinder;

    for<'a> fn(&'a ()) => ()
}


fn main(){}