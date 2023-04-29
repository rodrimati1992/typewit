use typewit::TypeEq;

fn covariance_a<'a, T>(te: TypeEq<T, &'static ()>) -> TypeEq<T, &'a ()> {
    te
}
fn covariance_b<'a, T>(te: TypeEq<&'static (), T>) -> TypeEq<&'a (), T> {
    te
}

fn contravariance_a<'a, T>(te: TypeEq<&'a (), T>) -> TypeEq<&'static (), T> {
    te
}
fn contravariance_b<'a, T>(te: TypeEq<T, &'a ()>) -> TypeEq<T, &'static ()> {
    te
}



fn main() {}

