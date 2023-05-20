#[doc(hidden)]
#[macro_export]
macro_rules! __parse_generics {
    (
        (
            $($callback:ident)::* !($($callback_args:tt)*)

            $generics:tt
        )
        $gen_args:tt
        $phantom_args:tt
        [$(,)+]
    ) => {
        $($callback)::* ! {
            $($callback_args)*
            $gen_args
            $phantom_args
            $generics
        }
    };
    (
        (
            $($callback:ident)::* !($($callback_args:tt)*)

            [$($($generics:tt)+)?]
        )
        $gen_args:tt
        $phantom_args:tt
        []
    ) => {
        $($callback)::* ! {
            $($callback_args)*
            $gen_args
            $phantom_args
            [$($($generics)+ ,)?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [$($prev_phantom_gen_args:tt)*]
        [
            $(
                $lt:lifetime $(:
                    $($lt_bound0:lifetime $( + $lt_bound1:lifetime)*)?
                )?
            ),+
            $(, $($ident:ident $($rem:tt)*)?)?
        ]
    ) => {
        $crate::__parse_generics!{
            $fixed 
            [$($prev_gen_args)* $($lt,)*]
            [$($prev_phantom_gen_args)* $($lt,)*]
            [$(, $($ident $($rem)*)? )?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [$($prev_phantom_gen_args:tt)*]
        [
            $(,)? $ty:ident $(:
                $($ty_bound0:lifetime $( + $ty_bound1:lifetime)*)?
            )?
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__parse_generics!{
            $fixed 
            [$($prev_gen_args)* $ty,]
            [$($prev_phantom_gen_args)* $ty,]
            [$(, $($rem)*)?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [$($prev_phantom_gen_args:tt)*]
        [
            $(,)? $ty:ident: $ty_bound2:ty
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__parse_generics!{
            $fixed 
            [$($prev_gen_args)* $ty,]
            [$($prev_phantom_gen_args)* $ty,]
            [$(, $($rem)*)?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        $prev_phantom_gen_args:tt
        [
            $(,)? const $const:ident: $const_ty:ty
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__parse_generics!{
            $fixed 
            [$($prev_gen_args)* $const,]
            $prev_phantom_gen_args
            [$(, $($rem)*)?]
        }
    };
}



#[doc(hidden)]
#[macro_export]
macro_rules! __trailing_comma {
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [] ) => {
        $($macro)::* !{$($args)* [$($prev)*] }
    };
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [,]) => {
        $($macro)::* !{$($args)* [$($prev)*,] }
    };
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [$t0:tt]) => {
        $($macro)::* !{$($args)* [$($prev)* $t0,] }
    };
    ($fixed:tt [$($prev:tt)*] [$t0:tt $($rem:tt)+]) => {
        $crate::__trailing_comma!{
            $fixed [$($prev)* $t0] [$($rem)*]
        }
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! __trailing_comma_until_semicolon {
    // fallback case
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [] ) => {
        $($macro)::* !{$($args)* [$($prev)*] }
    };
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [; $($rem:tt)*] ) => {
        $($macro)::* !{$($args)* [$($prev)*] $($rem)* }
    };
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [, ; $($rem:tt)*]) => {
        $($macro)::* !{$($args)* [$($prev)*,] $($rem)* }
    };
    ($fixed:tt [$($prev:tt)*] [$t0:tt $($rem:tt)*]) => {
        $crate::__trailing_comma_until_semicolon!{
            $fixed [$($prev)* $t0] [$($rem)*]
        }
    };
}