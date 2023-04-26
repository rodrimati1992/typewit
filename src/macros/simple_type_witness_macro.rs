/// Declares a type witness.
#[macro_export]
macro_rules! simple_type_witness {
    (
        $(#[$enum_meta:meta])*
        $vis:vis enum $enum:ident $([$($generics:tt)*])? 
        $(where[$($where:tt)*])?
        {
            $(
                $(#[$variant_meta:meta])*
                $variant:ident $([$($var_gen_args:tt)*])?
                $(where[$($vari_where:tt)*])?
                = $var_ty:ty
            ),*
            $(,)?
        }
    ) => {
        $crate::__stw_parse_generics!{
            (
                $(#[$enum_meta])*
                $vis enum $enum {
                    $((
                        $(#[$variant_meta])*
                        $variant $([$($var_gen_args)*])?
                        where[$($($vari_where)*)?]
                        = $var_ty
                    ))*
                }
                [$($($generics)*)?]
                where[$($($where)*)?]
            )
            []
            [$($($generics)*)?] 
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_parse_generics {
    (
        (
            $(# $enum_meta:tt)*
            $vis:vis enum $enum:ident $variants:tt
            $generics:tt
            where $where:tt
        )
        $gen_args:tt
        [$(,)+]
    ) => {
        $crate::__stw_where_clause_trailing_comma! {
            (
                $(# $enum_meta)*
                $vis enum $enum $generics $gen_args
                $variants
            )
            []
            $where
        }
    };
    (
        (
            $(# $enum_meta:tt)*
            $vis:vis enum $enum:ident $variants:tt
            [$($($generics:tt)+)?]
            where $where:tt
        )
        $gen_args:tt
        []
    ) => {
        $crate::__stw_where_clause_trailing_comma! {
            (
                $(# $enum_meta)*
                $vis enum $enum [$($($generics)+ ,)?] $gen_args
                $variants
            )
            []
            $where
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [
            $(
                $lt:lifetime $(:
                    $($lt_bound0:lifetime $( + $lt_bound1:lifetime)*)?
                )?
            ),+
            $(, $($ident:ident $($rem:tt)*)?)?
        ]
    ) => {
        $crate::__stw_parse_generics!{
            $fixed 
            [$($prev_gen_args)* $($lt,)*]
            [$(, $($ident $($rem)*)? )?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [
            $(,)? $ty:ident $(:
                $($ty_bound0:lifetime $( + $ty_bound1:lifetime)*)?
            )?
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__stw_parse_generics!{
            $fixed 
            [$($prev_gen_args)* $ty,]
            [$(, $($rem)*)?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [
            $(,)? $ty:ident: $ty_bound2:ty
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__stw_parse_generics!{
            $fixed 
            [$($prev_gen_args)* $ty,]
            [$(, $($rem)*)?]
        }
    };
    (
        $fixed:tt
        [$($prev_gen_args:tt)*]
        [
            $(,)? const $const:ident: $const_ty:ty
            $(, $($rem:tt)*)?
        ]
    ) => {
        $crate::__stw_parse_generics!{
            $fixed 
            [$($prev_gen_args)* $const,]
            [$(, $($rem)*)?]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_where_clause_trailing_comma {
    ( $fixed:tt [$($prev:tt)*] [] ) => {
        $crate::__stw_declare_enum!{ $fixed where [$($prev)*] }
    };
    ( $fixed:tt [$($prev:tt)*] [,]) => {
        $crate::__stw_declare_enum!{ $fixed where [$($prev)*,] }
    };
    ( $fixed:tt [$($prev:tt)*] [$t0:tt]) => {
        $crate::__stw_declare_enum!{ $fixed where [$($prev)* $t0,] }
    };
    ($fixed:tt [$($prev:tt)*] [$t0:tt $($rem:tt)+]) => {
        $crate::__stw_where_clause_trailing_comma!{
            $fixed [$($prev)* $t0] [$($rem)*]
        }
    };
}


#[doc(hidden)]
#[macro_export]
macro_rules! __stw_declare_enum {
    (
        (
            $(# $enum_meta:tt)*
            $vis:vis enum $enum:ident $generics:tt $gen_args:tt
            { $($variant_args:tt)* }
        )
        where $where:tt
    ) => {
        $crate::__stw_top_items!{
            $(# $enum_meta)*
            $vis enum $enum $generics $gen_args
            where $where

            { $($variant_args)* }
        }

        $(
            $crate::__stw_make_type_witness_impl!{
                $enum $generics $gen_args
                where $where
                $variant_args
            }
        )*
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __stw_top_items {
    (
        $(# $enum_meta:tt)*
        $vis:vis enum $enum:ident[$($generics:tt)*] [$($gen_args:tt)*]
        where [$($where:tt)*]

        {
            $((
                $(#[$variant_meta:meta])*
                $variant:ident
                $([$($var_gen_args:tt)*])?
                where $vari_where:tt
                = $var_ty:ty
            ))*
        }
    ) => {
        $(#$enum_meta)*
        $vis enum $enum <$($generics)* __Wit> 
        where $($where)*
        {
            $(
                $(#[$variant_meta])*
                $variant($crate::TypeEq<__Wit, $var_ty>),
            )*
        }

        impl<$($generics)* __Wit> $crate::TypeWitnessTypeArg for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {
            type Arg = __Wit;
        }
    }
}


#[doc(hidden)]
#[macro_export]
macro_rules! __stw_make_type_witness_impl {
    (
        $enum:ident[$($generics:tt)*] [$($gen_args:tt)*]
        where [$($where:tt)*]

        (
            $(#[$variant_meta:meta])*
            $variant:ident
            $([$($var_gen_args:tt),* $(,)?])?
            where[$($vari_where:tt)*]
            = $var_ty:ty
        )
    ) => {
        impl<$($generics)*> $crate::MakeTypeWitness for $enum<$($gen_args)* $var_ty>
        where
            $(
                $enum<$($var_gen_args,)* $var_ty>:
                    $crate::__::TypeIdentity<Type = Self>,
            )?
            $($where)*
            $($vari_where)*
        {
            const MAKE: Self = Self::$variant($crate::TypeEq::NEW);
        }

    }
}




