use typewit::TypeEq;

fn unconstrained_with_any_call<T, U>() -> Option<TypeEq<T, U>> {
    TypeEq::<T, U>::with_any()
}

fn main(){}