// Generates a macro that takes a sequence of tokens with balanced `<` and `>` tokens
// and collects it until one of the additional rules decides
macro_rules! declare_generics_consuming_macro {(
    $_:tt $gen_consuming_macro_:ident = $gen_consuming_macro:ident
    $parsing_where:expr;


    $($additional_rules:tt)*
) => {

    #[doc(hidden)]
    #[macro_export]
    macro_rules! $gen_consuming_macro_ {
        (
            $fixed:tt
            [$_($counter:tt)*]
            [$_($prev:tt)*]
            [< $_($rem:tt)*]
        ) => {
            $crate::__::$gen_consuming_macro!{
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
            $crate::__::$gen_consuming_macro!{
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
            $crate::__::$gen_consuming_macro!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >]
                [$_($rem)*]
            }
        };
        (
            $fixed:tt
            [$counter0:tt $_($counter:tt)*]
            [$_($prev:tt)*]
            [>> $_($rem:tt)*]
        ) => {
            $crate::__::$gen_consuming_macro!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >]
                [> $_($rem)*]
            }
        };
        (
            $fixed:tt
            [$counter0:tt $_($counter:tt)*]
            [$_($prev:tt)*]
            [>== $_($rem:tt)*]
        ) => {
            $crate::__::$gen_consuming_macro!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >]
                [== $_($rem)*]
            }
        };
        (
            $fixed:tt
            [$counter0:tt $_($counter:tt)*]
            [$_($prev:tt)*]
            [>= $_($rem:tt)*]
        ) => {
            $crate::__::$gen_consuming_macro!{
                $fixed
                [$_($counter)*]
                [$_($prev)* >]
                [= $_($rem)*]
            }
        };

        $($additional_rules)*

        (
            $fixed:tt
            $counter:tt
            [$_($prev:tt)*]
            [$token:tt $_($rem:tt)*]
        ) => {
            $crate::__::$gen_consuming_macro!{
                $fixed
                $counter
                [$_($prev)* $token]
                [$_($rem)*]
            }
        };
        ( $fixed:tt $counter:tt [$_($prev:tt)*] [$_($token0:tt $_($other:tt)*)?]) => {
            $crate::__::compile_error!{$crate::__::concat!(
                "unexpected end of ", $parsing_where,": `",
                stringify!($_($prev)* $_($token0)?),
                "`",
            )}
        };
    }

    pub use $gen_consuming_macro_ as $gen_consuming_macro;

}}

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
            // [$( ($generic_arg $phantom_arg = $default_val) )*]
            // Example of a single parameter list:
            // ('a (fn() -> &'a (),)) 
            // (T (fn() -> $crate::__::PhantomData<T>,)) 
            // (U (fn() -> $crate::__::PhantomData<U>,) = u32)
            // (N ()) 
            // (M () = 10)
            $gen_args:tt
            // [$( ($($generic_parameter:tt)*) )*]
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
                    [$_($prev_gen_args)* ($lt (fn() -> &$lt (),) )]
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



    declare_generics_consuming_macro! {
        $ __parse_ty_bounds_ = __parse_ty_bounds
        "bound";

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
    }
}} 

declare_parse_generics_macros!{$ [, >] [; where impl]}


pub use {
    __parse_in_generics_ as __parse_in_generics,
    __pg_parsed_ty_bounds_ as __pg_parsed_ty_bounds,
};


#[doc(hidden)]
#[macro_export]
macro_rules! __parse_generics {
    // angle bracket generics
    (
        $fixed:tt
        [< $($generics:tt)*]
    ) => {
        $crate::__::__parse_in_generics!{
            $fixed
            []
            []
            [$($generics)*]
        }
    };
    // square bracket generic params
    // note: this is accepted so that simple_type_witness 
    // can still parse square bracket generics.
    (
        $fixed:tt
        [[$($generics:tt)*] $($rem:tt)*]
    ) => {
        $crate::__::__parse_in_generics!{
            $fixed
            []
            []
            [$($generics)*> $($rem)*]
        }
    };
    // no generics case
    (
        (
            $($callback:ident)::* !($($callback_args:tt)*)
        )
        [$($rem:tt)*]
    ) => {
        $($callback)::* ! {
            $($callback_args)*
            []
            []
            $($rem)*
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __trailing_comma_for_where_clause {
    // fallback case
    (($($macro:ident)::* !($($args:tt)*)) [$($prev:tt)*] [] ) => {
        $($macro)::* !{$($args)* [$($prev)*] }
    };
    (($($macro:ident)::* !($($args:tt)*)) [$($($prev:tt)+)?] [$(,)? ; $($rem:tt)*] ) => {
        $($macro)::* !{$($args)* [$($($prev)+,)?] $($rem)* }
    };
    ($fixed:tt [$($prev:tt)*] [$t0:tt $($rem:tt)*]) => {
        $crate::__trailing_comma_for_where_clause!{
            $fixed [$($prev)* $t0] [$($rem)*]
        }
    };
}


// parses a where clause for an item where the where clause ends at any of:
// - `=`
// - `{...}`
// 
// The parsed tokens start with `where`, so that these can be parsed: 
// - there being no where clause
// - having a normal where clause
// - having a where clause delimited with brackets (e.g: `where[T: u32]`)
#[doc(hidden)]
#[macro_export]
macro_rules! __parse_where_clause_for_item {
    ($fixed:tt where [$($in_brackets:tt)*]: $($rem:tt)*) => {
        $crate::__::__parse_where_clause_for_item_inner!{
            $fixed [] [] [[$($in_brackets)*]: $($rem)*]
        }
    };
    // parses the `where [$where_predicates]` syntax that 
    // the simple_type_withness macro started with.
    ($fixed:tt where [$($in_brackets:tt)*] $($rem:tt)*) => {
        $crate::__trailing_comma_for_where_clause!{
            $fixed [] [$($in_brackets)*; $($rem)*]
        }
    };
    ($fixed:tt where $($rem:tt)*) => {
        $crate::__::__parse_where_clause_for_item_inner!{
            $fixed [] [] [$($rem)*]
        }
    };
    // no where clause
    (($($callback:ident)::* !($($callback_args:tt)*) ) $($rem:tt)*) => {
        $($callback)::* !{$($callback_args)* [] $($rem)*}
    };
}



declare_generics_consuming_macro! {
    $ __parse_where_clause_for_item_inner_ = __parse_where_clause_for_item_inner
    "where clause";

    // forward compatibility with `const { ... }` bounds,
    // dunno how likely const bounds are to be, but why not.
    ( $fixed:tt [] [$($prev:tt)*] [const {$($braced:tt)*} $($rem:tt)*] ) => {
        $crate::__::__parse_where_clause_for_item!{
            $fixed
            []
            [$($prev)* const {$($braced)*}]
            [$($rem)*]
        }
    };
    ( 
        ($($callback:ident)::* !($($callback_args:tt)*) )
        []
        [$($($prev:tt)+)?]
        [$(,)? = $($rem:tt)*] 
    ) => {
        $($callback)::* !{$($callback_args)* [$($($prev)+,)?] = $($rem)*}
    };
    (
        ($($callback:ident)::* !($($callback_args:tt)*) )
        []
        [$($($prev:tt)+)?]
        [$(,)? {$($braced:tt)*} $($rem:tt)*]
    ) => {
        $($callback)::* !{$($callback_args)* [$($($prev)+,)?] {$($braced)*} $($rem)*}
    };
    ($fixed:tt [] $prev:tt []) => {
        $crate::__::compile_error!{"unexpected end of where clause, expected rest of item"}
    };
}
