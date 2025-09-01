use typewit::TypeCmp;
use typewit::const_marker::{self as cm, ConstMarker, ConstMarkerEq};


const fn call_equals<L, R>() -> TypeCmp<L, R> {
    cm::CmEquals::<L, R>::VAL
}

const fn call_equals_with_unequal<L, R>() -> TypeCmp<L, R>
where
    L: ConstMarkerEq,
    R: ConstMarkerEq,
{
    cm::CmEquals::<L, R>::VAL
}



fn main() {}