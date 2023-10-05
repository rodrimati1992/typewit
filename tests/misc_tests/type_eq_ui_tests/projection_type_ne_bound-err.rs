use typewit::{TypeNe, TypeFn};

struct Mapper;

impl<T> TypeFn<T> for Mapper {
    type Output = [T; 1];
}

fn map_to_different_type<'a, L, R>(te: TypeNe<L, R>) -> TypeNe<[L; 1], [R; 1]> {
    te.map(Mapper)
}
fn project_to_different_type<'a, L, R>(te: TypeNe<L, R>) -> TypeNe<[L; 1], [R; 1]> {
    te.project::<Mapper>()
}


fn main() {}