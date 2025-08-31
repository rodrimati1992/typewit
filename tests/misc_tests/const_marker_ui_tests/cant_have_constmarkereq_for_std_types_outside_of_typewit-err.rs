use typewit::TypeCmp;
use typewit::const_marker::{ConstMarker, ConstMarkerEq};


struct Foo<const N: u8>;

impl<const N: u8> ConstMarker for Foo<N> {
    type Of = u8;
    const VAL: u8 = N;
}

impl<const N: u8> ConstMarkerEq for Foo<N> {
    type Equals<Rhs: ConstMarkerEq<Of = Self::Of>> = DumbEquals<Self, Rhs>;
}

struct DumbEquals<L, R>(L, R);

impl<L, R> ConstMarker for DumbEquals<L, R> {
    type Of = TypeCmp<L, R>;
    const VAL: Self::Of = unimplemented!();
}


fn main() {}
