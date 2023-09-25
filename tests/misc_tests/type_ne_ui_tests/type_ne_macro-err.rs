const _: () = {
    let _ = typewit::type_ne!{u8, u8};
    let _ = typewit::type_ne!{<T> T, T};
};

fn main() {}