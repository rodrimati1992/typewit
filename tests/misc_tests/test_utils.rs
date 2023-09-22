use typewit::Identity;


macro_rules! assertm {
    ($($tt:tt)*) => {assert!(matches!($($tt)*))};
} pub(crate) use assertm;


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


#[allow(dead_code)]
#[track_caller]
pub fn assert_not_type<T, Unexpected>(_: T) {
    assert_ne!(
        std::any::type_name::<T>(),
        std::any::type_name::<Unexpected>()
    );
}

#[track_caller]
pub fn assert_type_ne<T, Unexpected>(_: T, _: Unexpected) {
    assert_ne!(
        std::any::type_name::<T>(),
        std::any::type_name::<Unexpected>()
    );
}


pub struct AssertEq<L, R>(std::marker::PhantomData<(fn() -> L, fn() -> R)>)
where
    L: ?Sized + Identity<Type = R>,
    R: ?Sized;



