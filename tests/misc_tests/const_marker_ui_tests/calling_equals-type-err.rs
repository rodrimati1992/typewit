use typewit::TypeCmp;
use typewit::const_marker::{self as cm, ConstMarkerEq};


const fn call_equals<L, R>(l: &L, r: &R) -> TypeCmp<L, R> {
    cm::equals(l, r)
}

const fn call_equals_with_unequal<L, R>(l: &L, r: &R) -> TypeCmp<L, R>
where
    L: ConstMarkerEq,
    R: ConstMarkerEq,
{
    cm::equals(l, r)
}



fn main() {}