/// Declares a type-level function (struct that implements [`TypeFn`])
/// 
/// # Syntax
/// 
/// This section a `macro_rules!`-like syntax for the parameters that `type_fn` takes
/// ```text
/// $(#[$attrs:meta])*
/// $vis:vis struct $struct_name:ident $(< $struct_generics:generic_params >)?
/// $( where $struct_where_predicates:where_predicates  )?;
/// 
/// $(
///     $(#[$impl_attrs:meta])*
///     impl $(<$fn_generics:generic_params>)? $argument_type:ty => $return_type:ty
///     $( where $fn_where_predicates:where_predicates  )?
/// );+
/// 
/// $(;)?
/// ```
/// 
/// `:where_predicates` is a sequence of constraints.
/// e.g: `T: Foo, 'a: 'b, U: 'b`.
/// 
/// `:generic_params` is a list of generic parameter declarations.
/// e.g: `'a, T, const N: usize`.
/// 
#[macro_export]
macro_rules! type_fn {
    (
        $(#[$attrs:meta])*
        $vis:vis struct $struct_name:ident < $($rem:tt)*
    ) => {
        $crate::__::__parse_in_generics! {
            ($crate::__tyfn_parsed_capture_generics !((
                $(#[$attrs])*
                $vis struct $struct_name
            )))
            [] [] [$($rem)*]
        }
    };
    (
        $(#[$attrs:meta])*
        $vis:vis struct $struct_name:ident
        $($rem:tt)*
    ) => {
        $crate::__trailing_comma_until_semicolon!{
            ($crate::__tyfn_parsed_capture_where! (
                (
                    $(#[$attrs])*
                    $vis struct $struct_name [] []
                )
            ))
            []
            [$($rem)*]
        }
    };
    ($($rem:tt)*) => {
        $crate::__::compile_error!{
            "invalid argument for `type_fn` macro\n\
             expected struct declaration followed by type-level function definitions"
        }
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed_capture_generics {
    (
        ($($struct_stuff:tt)*)
        $capture_gen_args:tt
        $capture_generics:tt
        where $($rem:tt)*
    ) => {
        $crate::__trailing_comma_until_semicolon!{
            ($crate::__tyfn_parsed_capture_where! (
                ( $($struct_stuff)* $capture_gen_args $capture_generics )
            ))
            []
            [$($rem)*]
        }
    };
    (
        ($($struct_stuff:tt)*)
        $capture_gen_args:tt
        $capture_generics:tt
        ;$($rem:tt)*
    ) => {
        $crate::__tyfn_parsed_capture_where! {
            ( $($struct_stuff)* $capture_gen_args $capture_generics )
            []
            $($rem)*
        }
    };
    (
        $struct_stuff:tt
        $capture_gen_args:tt
        $capture_generics:tt
        $($first_token:tt $($rem:tt)*)?
    ) => {
        $crate::__::compile_error!{$crate::__::concat!(
            "expected `;` after struct definition",
            $( ", found `" $crate::__::stringify!($first_token), "`")?
        )}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed_capture_where {
    (
        ($($struct_stuff:tt)*)
        $captures_where:tt
        $($fns:tt)+
    ) => {
        $crate::__tyfn_parse_fns! {
            ( $($struct_stuff)* captures_where $captures_where )
            []
            [$($fns)*]
        }
    };
    (
        ($($struct_stuff:tt)*)
        [impl $($fns:tt)*]
    ) => {
        $crate::__::compile_error!{"expected `;` after struct declaration, found `impl`"}
    };
    (
        $struct_stuff:tt
        $where_predicates:tt
    ) => {
        $crate::__::compile_error!{"expected at least one type-level function definition"}
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parse_fns {
    (
        (
            $(#[$attrs:meta])*
            $vis:vis struct $struct_name:ident 

            $capture_gen_args:tt
            $capture_generics:tt
            captures_where $captures_where:tt
        )

        $fns:tt
        []
    ) => {
        $crate::__tyfn_split_capture_generics! {
            $fns

            $(#[$attrs])*
            $vis struct $struct_name 

            $capture_gen_args
            $capture_gen_args
            $capture_generics
            captures_where $captures_where
        }
    };
    (
        $fixed:tt
        $fns:tt
        [
            $(#[$impl_attrs:meta])*
            impl < $($rem:tt)*
        ]
    ) => {
        $crate::__::__parse_in_generics!{
            ($crate::__tyfn_parsed_fn_generics!(
                $fixed
                $fns
                [$(#[$impl_attrs])*]
            ))
            [] [] [$($rem)*]
        }
    };
    (
        $fixed:tt
        $fns:tt
        [
            $(#[$impl_attrs:meta])*
            impl $($rem:tt)*
        ]
    ) => {
        $crate::__tyfn_parsed_fn_generics!{
            $fixed
            $fns
            [$(#[$impl_attrs])*]
            [] [] $($rem)*
        }
    };
    (
        $fixed:tt
        $fns:tt
        [
            $(#[$impl_attrs:meta])*
            impl $type_fn_arg:ty => $ret_ty:ty
            where $($rem:tt)*
        ]
    ) => {
        $crate::__trailing_comma_until_semicolon!{
            ($crate::__tyfn_parsed_fn_where!(
                $fixed
                $fns
                [
                    $(#[$impl_attrs])*
                    impl[] $type_fn_arg => $ret_ty
                ]
            ))
            []
            [$($rem)*]
        }
    };
    (
        $fixed:tt
        [$($fns:tt)*]
        [
            $(#[$impl_attrs:meta])*
            impl $type_fn_arg:ty => $ret_ty:ty
            $(; $($rem:tt)*)?
        ]
    ) => {
        $crate::__tyfn_parse_fns!{
            $fixed
            [
                $($fns)*
                (
                    $(#[$impl_attrs])*
                    impl[] $type_fn_arg => $ret_ty
                    where[]
                )
            ]
            [$($($rem)*)?]
        }
    };
    (
        $fixed:tt
        $fns:tt
        [
            $(#[$impl_attrs:meta])*
            $type_fn_arg:ty => $($rem:tt)*
        ]
    ) => {
        $crate::__::compile_error!{$crate::__::concat!(
            "expected `impl`, found `",
            $crate::__::stringify!($type_fn_arg =>),
            "`\n",
            "helo: `impl ",
            $crate::__::stringify!($type_fn_arg =>),
            "` is likely to work."
        )}
    };
    (
        $fixed:tt
        $fns:tt
        [ $(#[$attrs:meta])* impl $arg:ty where $($rem:tt)* ]
    ) => {
        $crate::__::compile_error!{"where clauses for functions go after the return type"}
    };
    ( $fixed:tt [] [] ) => {
        $crate::__::compile_error!{"expected type-level function definitions"}
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed_fn_generics {
    (
        $fixed:tt
        [$($fns:tt)*]
        [$(#[$impl_attrs:meta])*]
        $__gen_args:tt
        $gen_params:tt
        $type_fn_arg:ty => $ret_ty:ty
        $(; $($rem:tt)*)?
    ) => {
        $crate::__tyfn_parse_fns!{
            $fixed
            [
                $($fns)*
                (
                    $(#[$impl_attrs])*
                    impl $gen_params $type_fn_arg => $ret_ty
                    where[]
                )
            ]
            [$($($rem)*)?]
        }
    };
    (
        $fixed:tt
        $fns:tt
        [$(#[$impl_attrs:meta])*]
        $__gen_args:tt
        $gen_params:tt
        $type_fn_arg:ty => $ret_ty:ty
        where $($rem:tt)*
    ) => {
        $crate::__trailing_comma_until_semicolon!{
            ($crate::__tyfn_parsed_fn_where!(
                $fixed
                $fns
                [
                    $(#[$impl_attrs])*
                    impl $gen_params $type_fn_arg => $ret_ty
                ]
            ))
            []
            [$($rem)*]
        }
    };
    (
        $fixed:tt
        $fns:tt
        $impl_attrs:tt
        $__gen_args:tt
        $gen_params:tt
        $type_fn_arg:ty where $($rem:tt)*
    ) => {
        $crate::__::compile_error!{"where clauses for functions go after the return type"}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed_fn_where {
    (
        $fixed:tt
        [$($fns:tt)*]
        [ $($fn_decl:tt)* ]
        $where_preds:tt

        $($rem:tt)*
    ) => {
        $crate::__tyfn_parse_fns!{
            $fixed
            [ $($fns)* ( $($fn_decl)* where $where_preds ) ]
            [$($rem)*]
        }
    };
}



#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_split_capture_generics {
    (
        $functions:tt
        
        $(#[$attrs:meta])*
        $vis:vis struct $struct_name:ident 

        $capture_gen_args:tt
        [$(($gen_arg:tt ($($($gen_phantom:tt)+)?) $($gen_rem:tt)*))*]
        $capture_generics:tt
        captures_where $captures_where:tt
    ) => {
        $crate::__tyfn_parsed!{
            $functions
            
            $(#[$attrs])*
            $vis struct $struct_name
            $capture_gen_args
            [$($(($($gen_phantom)+))?)*]
            $capture_generics
            $captures_where
        }
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __tyfn_parsed {
    (
        [$($functions:tt)+]
        
        $(#[$attrs:meta])*
        $vis:vis struct $function_name:ident
        $capt_gen_args:tt
        $capt_gen_phantom:tt
        $capt_generics:tt
        // where clause of the captures
        $captures_where:tt 
    ) => {
        $crate::__tyfn_declare_struct!{
            (
                $(#[$attrs])*
                $vis struct $function_name
            )
            $capt_gen_args
            $capt_gen_phantom
            $capt_generics
            where $captures_where
        }

        $(
            $crate::__tyfn_typefn_impl!{
                $functions

                $function_name

                $capt_gen_args
                $capt_generics
                where $captures_where 
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
            $vis:vis struct $function_name:ident
        )
        [$(($gen_arg:tt $ignored:tt $(= $gen_default:tt)?))*]
        [$($(@$has_phantom:tt)? $( ($($gen_phantom:tt)+) )+ )?]
        [$(($($gen_params:tt)*))*]
        where [$($($where:tt)+)?]
    ) => {
        $(#[$attrs])*
        $vis struct $function_name<$($($gen_params)* $(= $gen_default)?,)*> $((
            $($has_phantom)?
            $crate::__::PhantomData<($($($gen_phantom)*)*)>
        ))?
        $(where $($where)+)?;

        impl<$($($gen_params)*,)*> $function_name<$($gen_arg,)*> 
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
macro_rules! __tyfn_typefn_impl {
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
        > $crate::TypeFn<$ty_arg>
        for $function_name<$($capt_gen_args),*>
        where
            $($capt_where)*
            $($where_preds)*
        {
            type Output = $ret_ty;
        }
    };
}