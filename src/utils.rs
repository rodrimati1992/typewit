use core::mem::ManuallyDrop;

#[repr(C)]
pub union Transmuter<F, T> {
    pub from: ManuallyDrop<F>,
    pub to: ManuallyDrop<T>,
}

#[macro_export]
#[doc(hidden)]
macro_rules! __priv_transmute {
    ($from:ty, $to:ty, $value:expr) => {{
        $crate::__::ManuallyDrop::into_inner(
            $crate::utils::Transmuter::<$from, $to> {
                from: $crate::__::ManuallyDrop::new($value),
            }
            .to,
        )
    }};
}
