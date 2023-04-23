use typewit::TypeEq;

const fn unrelated_lifetime<'a, 'b>() -> TypeEq<&'a (), &'b ()> {
    TypeEq::NEW
}

const fn sublifetime<'a, 'b: 'a>() -> TypeEq<&'a (), &'b ()> {
    TypeEq::NEW
}


fn main(){}