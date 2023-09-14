
/// Declares an
/// [injective type-level function](crate::type_fn::InjTypeFn)
/// 
/// This macro takes in the exact same syntax as the [`type_fn`] macro.
/// 
/// This macro generates the same items as the `type_fn` macro,
/// in addition to [`RevTypeFn`], so that the function implements [`InjTypeFn`].
/// 
/// 
/// # Example
/// 
/// ### Basic
/// 
/// ```rust
/// use typewit::{CallFn, UncallFn, inj_type_fn};
/// 
/// // Calls the `ToSigned` function with `u64` as the argument.
/// let _: CallFn<ToSigned, u64> = 3i64;
/// 
/// // Gets the argument of the `ToSigned` function from the `i8` return value.
/// let _: UncallFn<ToSigned, i8> = 5u8;
/// 
/// inj_type_fn!{
///     struct ToSigned;
/// 
///     impl u128 => i128;
///     impl u64 => i64;
///     impl u32 => i32;
///     impl u16 => i16;
///     impl u8 => i8;
/// }
/// ```
/// 
/// [`type_fn`]: macro@crate::type_fn
/// [`TypeFn`]: crate::type_fn::TypeFn
/// [`InjTypeFn`]: crate::type_fn::InjTypeFn
/// [`RevTypeFn`]: crate::type_fn::RevTypeFn
#[macro_export]
macro_rules! inj_type_fn {
    ($($args:tt)*) => {
        $crate::__type_fn!{
            __tyfn_injtypefn_impl
            $($args)*
        }
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_injtypefn_impl {
    (
        (
            $(#[$attrs:meta])*
            $impl:ident[$(($($fn_gen_param:tt)*))*] $ty_arg:ty => $ret_ty:ty
            where[ $($where_preds:tt)* ] 
        )

        $function_name:ident

        [$(($capt_gen_args:tt $($rem_0:tt)*))*]
        [
            $(($capt_lt:lifetime $($capt_lt_rem:tt)*))*
            $(($capt_tcp:ident $($capt_tcp_rem:tt)*))*
        ]
        where [$($capt_where:tt)*]
        
    ) => {
        $(#[$attrs])*
        #[allow(unused_parens)]
        $impl<
            $($capt_lt $($capt_lt_rem)*,)*
            $($($fn_gen_param)*,)*
            $($capt_tcp $($capt_tcp_rem)*,)*
        > $crate::TypeFn<$ty_arg> 
        for $function_name<$($capt_gen_args),*>
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Output = $ret_ty;
        }

        $(#[$attrs])*
        #[allow(unused_parens)]
        $impl<
            $($capt_lt $($capt_lt_rem)*,)*
            $($($fn_gen_param)*,)*
            $($capt_tcp $($capt_tcp_rem)*,)*
        > $crate::type_fn::RevTypeFn<$ret_ty> 
        for $function_name<$($capt_gen_args),*>
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Arg = $ty_arg;
        }
    };
}