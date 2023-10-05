use typewit::{TypeEq, TypeNe, TypeFn};

struct Mapper;

impl<T> TypeFn<T> for Mapper {
    type Output = [T; 1];
}
impl<T> typewit::RevTypeFn<[T; 1]> for Mapper {
    type Arg = T;
}



fn flipper<L, R>(te: TypeNe<L, R>) -> TypeNe<L, R> {
    te.flip()
}

fn joiner_right<A, B, C>(first: TypeNe<A, B>, second: TypeEq<B, C>) -> TypeNe<(), ()> {
    first.join_right(second)
}

fn joiner_left<A, B, C>(first: TypeNe<A, B>, second: TypeEq<C, A>) -> TypeNe<(), ()> {
    first.join_left(second)
}

fn map_to_different_type<'a, L, R>(te: TypeNe<L, R>) -> TypeNe<u8, u16> {
    te.map(Mapper)
}
fn project_to_different_type<'a, L, R>(te: TypeNe<L, R>) -> TypeNe<u8, u16> {
    te.project::<Mapper>()
}
fn unmap_to_different_type<'a, L, R>(te: TypeNe<[L; 1], [R; 1]>) -> TypeNe<u8, u16> {
    te.unmap(Mapper)
}
fn unproject_to_different_type<'a, L, R>(te: TypeNe<[L; 1], [R; 1]>) -> TypeNe<u8, u16> {
    te.unproject::<Mapper>()
}


fn main() {}