use typewit::TypeEq;

const _: () = { TypeEq::<(), u8>::NEW; };

const fn new_assoc_const_with_differnt_types() -> TypeEq<i8, u8> {
    TypeEq::NEW
}

const fn new_function_with_differnt_types() -> TypeEq<i8, u8> {
    TypeEq::new()
}


fn main(){}