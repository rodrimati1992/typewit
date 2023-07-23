use typewit::TypeEq;

const _: () = { TypeEq::<(), u8>::NEW; };

const fn new_assoc_const_with_differnt_types() -> TypeEq<i8, u8> {
    TypeEq::NEW
}

const fn new_function_with_differnt_types() -> TypeEq<i8, u8> {
    TypeEq::new()
}

const fn coercing_to_dyn_right() -> TypeEq<u8, dyn std::fmt::Debug> {
    TypeEq::new::<u8>()
}

const fn coercing_to_dyn_left() -> TypeEq<dyn std::fmt::Debug, u8> {
    TypeEq::new::<u8>()
}

const fn coercing_to_dyn_botht() -> TypeEq<dyn std::fmt::Debug, dyn std::fmt::Debug> {
    TypeEq::new::<u8>()
}


fn main(){}