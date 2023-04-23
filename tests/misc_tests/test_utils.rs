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