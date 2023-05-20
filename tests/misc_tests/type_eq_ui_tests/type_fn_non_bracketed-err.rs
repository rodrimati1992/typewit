typewit::type_fn!{
    struct WithTypeParam<T>;
    () => ()
}

typewit::type_fn!{
    struct NakedForBinder;

    for<'a> fn(&'a ()) => ()
}


fn main(){}