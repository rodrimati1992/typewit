
/// 
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
            impl[$(($($fn_gen_param:tt)*))*] $ty_arg:ty => $ret_ty:ty
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
        impl<
            $($capt_lt $($capt_lt_rem)*,)*
            $($($fn_gen_param)*,)*
            $($capt_tcp $($capt_tcp_rem)*,)*
        > $crate::type_fn::InjTypeFnArg<$function_name<$($capt_gen_args),*>> 
        for ($ty_arg,)
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Ret = ($ret_ty,);
        }

        $(#[$attrs])*
        #[allow(unused_parens)]
        impl<
            $($capt_lt $($capt_lt_rem)*,)*
            $($($fn_gen_param)*,)*
            $($capt_tcp $($capt_tcp_rem)*,)*
        > $crate::type_fn::InjTypeFnRet<$function_name<$($capt_gen_args),*>> 
        for ($ret_ty,)
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Arg = ($ty_arg,);
        }
    };
}