use typewit::{TypeEq, TypeFn};

fn flipper<L, R>(te: TypeEq<L, R>) -> TypeEq<L, R> {
    te.flip()
}

fn joiner_a<A, B, C>(first: TypeEq<A, B>, second: TypeEq<B, C>) -> TypeEq<A, B> {
    first.join(second)
}

fn joiner_b<A, B, C>(first: TypeEq<A, B>, second: TypeEq<C, B>) {
    let _ = first.join(second);
}

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
fn unmap_to_different_type<'a, L, R>(te: TypeEq<[L; 1], [R; 1]>) -> TypeEq<u8, u16> {
    te.unmap(Mapper)
}
fn unproject_to_different_type<'a, L, R>(te: TypeEq<[L; 1], [R; 1]>) -> TypeEq<u8, u16> {
    te.unproject::<Mapper>()
}

struct Mapper;

impl<T> TypeFn<T> for Mapper {
    type Output = [T; 1];
}
impl<T> typewit::RevTypeFn<[T; 1]> for Mapper {
    type Arg = T;
}


#[cfg(feature = "const_marker")]
use typewit::const_marker::Usize;

#[cfg(feature = "const_marker")]
fn in_array_to_different_type<'a, L, R>(
    te: TypeEq<L, R>
) -> TypeEq<[u8; 1], [u16; 1]> {
    te.in_array(TypeEq::new::<Usize<1>>())
}

#[cfg(feature = "const_marker")]
fn in_array_to_different_len<'a, L, R, const A: usize, const B: usize>(
    te: TypeEq<L, R>,
    te_len: TypeEq<Usize<A>, Usize<B>>,
) -> TypeEq<[L; 1], [R; 1]> {
    te.in_array(te_len)
}

fn main() {}