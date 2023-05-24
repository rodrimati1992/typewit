#[track_caller]
pub fn assert_type<T, Expected>(_: T) {
    assert_eq!(
        std::any::type_name::<T>(),
        std::any::type_name::<Expected>()
    );
}

#[track_caller]
pub fn assert_type_eq<T, Expected>(_: T, _: Expected) {
    assert_eq!(
        std::any::type_name::<T>(),
        std::any::type_name::<Expected>()
    );
}


pub trait TypeIdentity {
    type Type: ?Sized;
}

impl<T: ?Sized> TypeIdentity for T {
    type Type = T;
}

pub struct AssertEq<L, R>(std::marker::PhantomData<(fn() -> L, fn() -> R)>)
where
    L: ?Sized + TypeIdentity<Type = R>,
    R: ?Sized;



