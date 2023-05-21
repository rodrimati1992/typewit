macro_rules! declare_parse_generics_macros {($_:tt [$($sep:tt)*] [$($err_token:tt)*]) => {
    #[doc(hidden)]
    #[macro_export]
    macro_rules! __parse_in_generics_ {
        $(
            ($fixed:tt $gen_args:tt $gen:tt [$_(,)? $err_token $_($rem:tt)*]) => {
                $crate::__::compile_error!{$crate::__::concat!(
                    "unexpected `",
                    $crate::__::stringify!($err_token),
                    "` in generic parameter list",
                )}
            };
        )*
        (
            ( $_($callback:ident)::* !($_($callback_args:tt)*) )
            // ($generic_arg $phantom_arg = $default_val)
            // Example of a single parameter list:
            // ('a (fn() -> &'a (),)) 
            // (T (fn() -> $crate::__::PhantomData<T>,)) 
            // (U (fn() -> $crate::__::PhantomData<U>,) = u32)
            // (N ()) 
            // (M () = 10)
            $gen_args:tt
            $generics:tt
            [$_(,)* $_(> $_($rem:tt)*)?]
        ) => {
            $_($callback)::* ! {
                $_($callback_args)*
                $gen_args
                $generics
                $_($_($rem)*)?
            }
        };
        $(
            (
                $fixed:tt
                [$_($prev_gen_args:tt)*]
                [$_($prev_gen:tt)*]
                [
                    $_(,)? $lt:lifetime $_(:
                        $_($lt_bound0:lifetime $_( + $lt_bound1:lifetime)*)?
                    )?
                    $_($sep $_($rem:tt)*)?
                ]
            ) => {
                $crate::__::__parse_in_generics!{
                    $fixed 
                    [$_($prev_gen_args)* ($lt (fn() -> &'a (),) )]
                    [$_($prev_gen)* ($lt $_(: $_($lt_bound0 $_( + $lt_bound1 )* )?)?)]
                    [$_($sep  $_($rem)*)?]
                }
            };
            (
                $fixed:tt
                [$_($prev_gen_args:tt)*]
                [$_($prev_gen:tt)*]
                [
                    $_(,)? const $const:ident: $const_ty:ty $_(= $default:tt)?
                    $_($sep $_($rem:tt)*)?
                ]
            ) => {
                $crate::__::__parse_in_generics!{
                    $fixed 
                    [$_($prev_gen_args)* ($const () $_(= $default)?)]
                    [$_($prev_gen)* (const $const: $const_ty)]
                    [$_($sep $_($rem)*)?]
                }
            };
        )*
        (
            $fixed:tt
            $prev_gen_args:tt
            $prev_gen:tt
            [
                $_(,)? $ty:ident: $_($rem:tt)*
            ]
        ) => {
            $crate::__::__parse_ty_bounds!{
                (
                    $fixed 
                    $prev_gen_args
                    $prev_gen
                    $ty
                )
                [] // counter for depth between < > pairs
                []
                [$_($rem)*]
            }
        };
        (
            $fixed:tt
            $prev_gen_args:tt
            $prev_gen:tt
            [
                $_(,)? $ty:ident $_($rem:tt)*
            ]
        ) => {
            $crate::__::__pg_parsed_ty_bounds!{
                $fixed 
                $prev_gen_args
                $prev_gen
                $ty
                []
                $_($rem)*
            }
        };
        ($fixed:tt $gen_args:tt $gen:tt []) => {
            $crate::__::compile_error!{"unexpected end of generic parameter list"}
        };
        ($fixed:tt $gen_args:tt $gen:tt [$token0:tt $_($token1:tt $_($other:tt)*)?]) => {
            $crate::__::compile_error!{$crate::__::concat!(
                "unexpected token(s) in generic parameter list: `",
                stringify!($token0 $_($token1)?)
                "`"
            )}
        };
    }

    #[doc(hidden)]
    #[macro_export]
    macro_rules! __pg_parsed_ty_bounds_ {
        $(
            ($fixed:tt $gen_args:tt $gen:tt $ty:tt $bound:tt [$err_token $_($rem:tt)*]) => {
                $crate::__::compile_error!{$crate::__::concat!(
                    "unexpected `",
                    $crate::__::stringify!($err_token),
                    "` in type parameter declaration",
                )}
            };
        )*

        $(
            (
                $fixed:tt
                [$_($prev_gen_args:tt)*]
                [$_($prev_gen:tt)*]
                $ty:ident 
                [$_($_($bound:tt)+)?]
                $_(= $default:ty)? $sep $_($rem:tt)*
            ) => {
                $crate::__::__parse_in_generics!{
                    $fixed 
                    [
                        $_($prev_gen_args)* 
                        ($ty (fn() -> $crate::__::PhantomData<$ty>,) $_(= $default)?)
                    ]
                    [$_($prev_gen)* ($ty $_(: $_($bound)+)?)]
                    [$sep $_($rem)*]
                }
            };
        )*
    }

    #[doc(hidden)]
    #[macro_export]
    macro_rules! __parse_ty_bounds_ {
        (
            $fixed:tt
            [$_($counter:tt)*]
            [$_($prev:tt)*]
            [< $_($rem:tt)*]
        ) => {
            $crate::__::__parse_ty_bounds!{
                $fixed
                [1 $_($counter)*]
                [$_($prev)* <]
                [$_($rem)*]
            }
        };
        (
            $fixed:tt
            [$_($counter:tt)*]
            [$_($prev:tt)*]
            [<< $_($rem:tt)*]
        ) => {
            $crate::__::__parse_ty_bounds!{
                $fixed
                [1 1 $_($counter)*]
                [$_($prev)* <<]
                [$_($rem)*]
            }
        };
        (
            $fixed:tt
            [$counter0:tt $_($counter:tt)*]
            [$_($prev:tt)*]
            [> $_($rem:tt)*]
        ) => {
            $crate::__::__parse_ty_bounds!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >]
                [$_($rem)*]
            }
        };
        (
            ($_($fixed:tt)*)
            [$counter0:tt]
            [$_($prev:tt)*]
            [>> $_($rem:tt)*]
        ) => {
            $crate::__::__pg_parsed_ty_bounds!{ $_($fixed)* [$_($prev)* >] > $_($rem)* }
        };
        (
            $fixed:tt
            [$counter0:tt $counter1:tt $_($counter:tt)*]
            [$_($prev:tt)*]
            [>> $_($rem:tt)*]
        ) => {
            $crate::__::__parse_ty_bounds!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >>]
                [$_($rem)*]
            }
        };
        ( ($_($fixed:tt)*) [] $prev:tt [$_(= $_($rem:tt)*)?] ) => {
            $crate::__::__pg_parsed_ty_bounds!{ $_($fixed)* $prev $_(= $_($rem)*)? }
        };

        $(
            ( ($_($fixed:tt)*) [] $prev:tt [$sep $_($rem:tt)*] ) => {
                $crate::__::__pg_parsed_ty_bounds!{ $_($fixed)* $prev $sep $_($rem)* }
            };
        )*

        $(
            ($fixed:tt $count:tt [$_($prev:tt)*] [$err_token $_($rem:tt)*]) => {
                $crate::__::compile_error!{$crate::__::concat!(
                    "unexpected end of bound: `",
                    stringify!($_($prev)* $err_token),
                    "`",
                )}
            };
        )*

        (
            $fixed:tt
            $counter:tt
            [$_($prev:tt)*]
            [$token:tt $_($rem:tt)*]
        ) => {
            $crate::__::__parse_ty_bounds!{
                $fixed
                $counter
                [$_($prev)* $token]
                [$_($rem)*]
            }
        };
        ( $fixed:tt $counter:tt [$_($prev:tt)*] [$_($token0:tt $_($other:tt)*)?]) => {
            $crate::__::compile_error!{$crate::__::concat!(
                "unexpected end of bound: `",
                stringify!($_($prev)* $_($token0)?),
                "`",
            )}
        };
    }
}} 

declare_parse_generics_macros!{$ [, >] [; where impl]}


pub use {
    __parse_in_generics_ as __parse_in_generics,
    __pg_parsed_ty_bounds_ as __pg_parsed_ty_bounds,
    __parse_ty_bounds_ as __parse_ty_bounds,
};
