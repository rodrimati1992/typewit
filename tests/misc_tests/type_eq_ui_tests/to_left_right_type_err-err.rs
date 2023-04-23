use typewit::TypeEq;


fn to_left_err<L, R>(te: TypeEq<L, R>, left: impl Fn() -> L, right: impl Fn() -> R) {
    let _: R = te.to_left(right());
    let _: R = te.to_left(left());
}

fn to_right_err<L, R>(te: TypeEq<L, R>, left: impl Fn() -> L, right: impl Fn() -> R) {
    let _: L = te.to_right(right());
    let _: L = te.to_right(left());
}




fn main() {}