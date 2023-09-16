use typewit::TypeNe;

fn unconstrained_with_any_call<T, U>() -> Option<TypeNe<T, U>> {
    TypeNe::<T, U>::with_any()
}



fn main() {}