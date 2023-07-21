typewit::type_fn!{
    struct Unit;
}

typewit::type_fn!{
    impl u8 => u16;
}

typewit::type_fn!{
    struct NoImplToken;

    () => ()
}


typewit::type_fn!{
    struct NoSemicolon
}

typewit::type_fn!{
    struct ImplAfterNoSemicolon

    impl () => ()
}


typewit::type_fn!{
    struct UnfinishedGenericParams0<;
}

typewit::type_fn!{
    struct UnfinishedGenericParams1<T,;
}

typewit::type_fn!{
    struct UnfinishedGenericParams2Impl<T: Add<Type<u16>>;

    impl () => T::Output
}


typewit::type_fn!{
    struct UnfinishedGenericParams0Impl<impl
}

typewit::type_fn!{
    struct UnfinishedGenericParams1Impl<T,impl
}

typewit::type_fn!{
    struct UnfinishedGenericParams2Impl<T: Add<Type<u16>> impl

    impl () => T::Output
}

typewit::type_fn!{
    struct GenParamAttributes<#[foo] #[bar] T>;

    impl () => ()
}

typewit::type_fn!{
    struct CfgAllOnImplGeneric;

    impl<#[cfg(all())] T> () => u8
}


fn main(){}