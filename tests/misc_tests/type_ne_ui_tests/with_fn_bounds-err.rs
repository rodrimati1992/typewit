use typewit::TypeNe;

typewit::inj_type_fn! {
    struct MakeNe<T, U>;

    impl u32 => Option<T>;
    impl typewit::type_ne::LeftArg => [U; 0];
}

const _: TypeNe<[i16; 0], Option<i8>> = TypeNe::with_fn(MakeNe::NEW);

fn main() {}