typewit::inj_type_fn!{
    struct FromTooGenericA;

    impl<T, const N: usize> [T; N] => T;
}

typewit::inj_type_fn!{
    struct FromTooGenericB;

    impl<T, U> (T, U) => T;
}

typewit::inj_type_fn!{
    struct IntoIteratorFn;

    impl<I: IntoIterator> I => <I as IntoIterator>::Item;
}

fn main(){}