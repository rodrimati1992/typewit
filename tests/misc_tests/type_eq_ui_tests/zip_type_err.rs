use typewit::{TypeEq, TypeFn};


fn zip2<'a, L, R>(te: TypeEq<L, R>) -> TypeEq<L, R> {
    te.zip(te)
}

fn zip3<'a, A, B, C, D, E, F>(
    a: TypeEq<A, B>,
    b: TypeEq<C, D>,
    c: TypeEq<E, F>,
) -> TypeEq<(), ()> {
    a.zip3(b, c)
}

fn zip4<'a, A, B, C, D, E, F, G, H>(
    a: TypeEq<A, B>,
    b: TypeEq<C, D>,
    c: TypeEq<E, F>,
    d: TypeEq<G, H>,
) -> TypeEq<(), ()> {
    a.zip4(b, c, d)
}



fn main() {}