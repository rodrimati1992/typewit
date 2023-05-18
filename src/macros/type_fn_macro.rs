/// TODO
#[macro_export]
macro_rules! type_fn {
    (
        $(
            captures($($capture_generics:tt)*)
            $(where[$($captures_where:tt)*])?
        )?

        $(
            $(#[$attrs:meta])*
            $(pub $(($($vis:tt)*))?)? 
            fn $function:ident $([$($gen_params:tt)*])? ($type_fn_arg:ty)
            $(where[ $($where_fn:tt)* ])?
            { $ret_ty:ty }
        )+
    ) => {
        $crate::__parse_generics!{
            (
                $crate::__tyfn_parsed_capture_generics! (
                    [
                        $((
                            $(#[$attrs])*
                            $(pub $(($($vis)*))?)? 
                            fn $function[$($($gen_params)*)?] ($type_fn_arg)
                            where[ $($($where_fn)*)? ]
                            { $ret_ty }
                        ))+
                    ]
                    captures_where[$($($($captures_where)*)?)?]
                )

                [$($($capture_generics)*)?]
            )

            []
            []
            [$($($capture_generics)*)?]
        }
    };
}



#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed_capture_generics {
    (
        [$first_fn:tt $($functions:tt)*]
        captures_where $captures_where:tt
        $capt_gen_args:tt
        $capt_phantom_args:tt
    
        [
            $(
                $lt:lifetime $(:
                    $($lt_bound0:lifetime $( + $lt_bound1:lifetime)*)?
                )?,
            )*
            $($ty_const_param:ident $($ty_const_rem:tt)*)?
        ]
    ) => {
        $crate::__trailing_comma !{
            (
                $crate::__tyfn_parsed!(
                    $first_fn
                    [$first_fn $($functions)*]
                    $capt_gen_args
                    $capt_phantom_args
                    
                    [$( $lt $(: $($lt_bound0 $( + $lt_bound1)*)? )?, )*]
                    [$($ty_const_param $($ty_const_rem)*)?]
                )
            )
            []
            $captures_where
        }
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed {
    (
        (
            $(#[$attrs:meta])*
            $vis:vis fn $function_name:ident $($rem:tt)*
        )
        [$($functions:tt)+]
        $capt_gen_args:tt
        [$($($capt_phantom_args:tt)+)?]
        $capt_lt_params:tt
        $capt_ty_const_params:tt
        // where clause of the captures
        $captures_where:tt 
    ) => {
        $crate::__tyfn_declare_struct!{
            (
                $(#[$attrs])*
                $vis fn $function_name
            )
            $capt_gen_args
            [$(($($capt_phantom_args)+))?]
            $capt_lt_params
            $capt_ty_const_params
            where $captures_where
        }

        $(
            $crate::__tyfn_typefn_impl_outer!{
                $functions
                $functions

                $function_name

                (
                    $capt_lt_params
                    $capt_ty_const_params
                    where $captures_where 
                    
                    $capt_gen_args
                )
            }
        )*
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_declare_struct {
    (
        (
            $(#[$attrs:meta])*
            $vis:vis fn $function_name:ident
        )
        [$($capt_gen_args:tt)*]
        [$($(@$has_phantom:tt)? ($($lt_arg:lifetime,)* $($ty_arg:ident,)*))?]
        [$($lt_param:tt)*]
        [$($ty_const_param:tt)*]
        where [$($($where:tt)+)?]
    ) => {
        $(#[$attrs])*
        $vis struct $function_name<$($lt_param)* $($ty_const_param)*> $((
            $crate::__::PhantomData<(
                ($(fn() -> &$lt_arg (),)*),
                ($(fn() -> $ty_arg,)*),
            )>
        ))?
        $(where $($where)+)?;

        impl<$($lt_param)* $($ty_const_param)*> $function_name<$($capt_gen_args)*> 
        $(where $($where)+)?
        {
            #[doc = $crate::__::concat!(
                "Constructs a `", $crate::__::stringify!($function_name), "`"
            )]
            $vis const NEW: Self = Self $($($has_phantom)? ($crate::__::PhantomData))?;
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_typefn_impl_outer {
    (
        (
            $(#[$attrs:meta])*
            $vis:vis fn $function:ident [$($gen_params:tt)*] $($rem_fn:tt)*
        )
        $($rem_params:tt)+
    ) => {
        $crate::__trailing_comma !{
            (
                $crate::__tyfn_typefn_impl!(
                    $($rem_params)*
                )
            )

            []
            [$($gen_params)*]
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_typefn_impl {
    (
        (
            $(#[$attrs:meta])*
            $vis:vis fn $function:ident $gen_params:tt ($ty_arg:ty)
            where[ $($where_preds:tt)* ] 
            {
                $ret_ty:ty
            }
        )

        $function_name:ident

        (
            [$($capt_lt_param:tt)*]
            [$($capt_ty_const_param:tt)*]
            where [$($capt_where:tt)*]
            [$($capt_gen_args:tt)*]
        )

        [$($fn_gen_param:tt)*]
    ) => {
        impl<$($capt_lt_param)* $($fn_gen_param)* $($capt_ty_const_param)*>
            $crate::TypeFn<$ty_arg>
        for $function_name<$($capt_gen_args)*>
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Output = $ret_ty;
        }

    };
}