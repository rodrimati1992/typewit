use typewit::{RevTypeFn, TypeFn};

struct NonInjective;

impl<T> TypeFn<T> for NonInjective {
    type Output = u8;

    const TYPE_FN_ASSERTS: () = { let _: typewit::CallInjFn<Self, T>; };
}

impl RevTypeFn<u8> for NonInjective {
    type Arg = ();
}


fn main(){}