use typewit::{TypeEq, TypeFn};


fn in_ref_to_different_type<L, R>(te: TypeEq<L, R>) -> TypeEq<&'static u8, &'static u16> {
    te.in_ref()
}

fn in_mut_to_different_type<'a, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a mut u8, &'a mut u16> {
    te.in_mut()
}

fn map_to_different_type<'a, L, R>(te: TypeEq<L, R>) -> TypeEq<u8, u16> {
    te.map(Mapper)
}

fn project_to_different_type<'a, L, R>(te: TypeEq<L, R>) -> TypeEq<u8, u16> {
    te.project::<Mapper>()
}

struct Mapper;

impl<T> TypeFn<T> for Mapper {
    type Output = [T; 1];
}



fn main() {}