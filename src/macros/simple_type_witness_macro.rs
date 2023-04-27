/// Declares a [type witness](crate#what-are-type-witnesses) enum.
/// 
#[doc = explain_type_witness!()]
/// 
/// # Generated items
/// 
/// This macro generates:
/// 
/// - An enum with tuple variants, each of which has a single [`TypeEq`] field.
/// 
/// - Impls of [`Copy`] and [`Clone`] for the enum.
/// 
/// - An impl of [`TypeWitnessTypeArg`] for the enum.
/// 
/// - An impl of [`MakeTypeWitness`](crate::MakeTypeWitness) for each variant of the enum.
/// 
/// # Syntax
/// 
/// This macro uses square brackets to make parsing generics and where clauses easier.
/// 
/// This macro takes an enum-like syntax:
/// ```text
///     $(#[$enum_meta:meta])*
///     $vis:vis enum $enum:ident $([$($generics:tt)*])? 
///     // The where clause of the enum
///     $(where[$($where:tt)*])?
///     {
///         $(
///             $(#[$variant_meta:meta])*
///             $variant:ident $([$($var_gen_args:tt)*])?
///             // additional bounds for the MakeTypeWitness impl that constructs this variant.
///             $(where[$($vari_where:tt)*])?
///             // the type this variant requires the implicit `__Wit` type parameter to be.
///             = $witnessed_ty:ty
///         ),*
///         $(,)?
///     }
/// ```
/// `[$($generics:tt)*]`(optional parameter) are the generic parameters of the enum,
/// to which this macro adds an implicit `__Wit` type parameter that the variants constrain.
/// 
/// `[$($var_gen_args:tt)*]`(optional parameter)[(example usage)](#var_gen_args-example): 
/// this parameter overrides the generic arguments of the enum in its 
/// [`MakeTypeWitness`] implementation.
/// 
/// ### Limitations
/// 
/// This macro cannot parse defaulted generic parameters,
/// doing so would either require procedural macros or 
/// a very complex `macro_rules!` macro.
/// 
/// 
/// # Examples
/// 
/// ### Basic
/// 
/// This example demonstrates a basic usage of this macro
/// 
/// ```rust
/// use typewit::MakeTypeWitness;
/// 
/// assert_eq!(do_it(1), 1);
/// assert_eq!(do_it(2), 4);
/// assert_eq!(do_it(3), 9);
/// assert_eq!(do_it("foo"), 3);
/// assert_eq!(do_it("hello"), 5);
/// 
/// const fn do_it<'a, T>(arg: T) -> usize 
/// where
///     Witness<'a, T>: MakeTypeWitness,
/// {
///     match MakeTypeWitness::MAKE {
///         // `te` is a `TypeEq<T, u8>`, `te.to_right(arg)` goes from `T` to `u8.`
///         Witness::U8(te) => (te.to_right(arg) as usize).pow(2),
///
///         // `te` is a `TypeEq<T, &'a str>`, `te.to_right(arg)` goes from `T` to `&'a str.`
///         Witness::Str(te) => te.to_right(arg).len(),
///     }
/// }
/// 
/// typewit::simple_type_witness! {
///     // Generic parameters go inside square brackets (these are optional),
///     // the macro implicitly adds a `__Wit` type parameter after all generics.
///     enum Witness['a] {
///         // This variant requires `__Wit == u8`
///         U8 = u8,
///         // This variant requires `__Wit == &'a str`
///         Str = &'a str,
///     }
/// }
/// ```
/// the above invocation of `simple_type_witness` effectively generates this code:
/// ```rust
/// enum Witness<'a, __Wit> {
///     U8(typewit::TypeEq<__Wit, u8>),
///     Str(typewit::TypeEq<__Wit, &'a str>),
/// }
/// impl<'a, __Wit> typewit::TypeWitnessTypeArg for Witness<'a, __Wit> {
///     type Arg = __Wit;
/// }
/// impl<'a> typewit::MakeTypeWitness for Witness<'a, u8> {
///     const MAKE: Self = Self::U8(typewit::TypeEq::NEW);
/// }
/// impl<'a> typewit::MakeTypeWitness for Witness<'a, &'a str> {
///     const MAKE: Self = Self::Str(typewit::TypeEq::NEW);
/// }
/// ```
/// (consult the [generated items] section for all the generated impls)
/// 
/// ### where clauses 
/// 
/// This example demonstrates the where clause syntax
/// ```rust
/// # use std::fmt::Debug;
/// typewit::simple_type_witness! {
///     // Generic parameters go inside square brackets (these are optional),
///     // the macro implicitly adds a `__Wit` type parameter after all generics.
///     enum Witness['a, T] 
///     // the constraints in the where clause go inside square brackets 
///     where[T: Debug] 
///     {
///         // This variant requires `__Wit == T`.
///         // The `MakeTypeWitness` impl for this variant also requires `T: Copy`.
///         Value where [T: Copy] = T,
///         // This variant requires `__Wit == &'a T`
///         Ref = &'a T,
///     }
/// }
/// ```
/// the above invocation of `simple_type_witness` effectively generates this code:
/// ```rust
/// # use std::fmt::Debug;
/// enum Witness<'a, T, __Wit>
/// where
///     T: Debug,
/// {
///     Value(typewit::TypeEq<__Wit, T>),
///     Ref(typewit::TypeEq<__Wit, &'a T>),
/// }
/// impl<'a, T, __Wit> typewit::TypeWitnessTypeArg for Witness<'a, T, __Wit>
/// where
///     T: Debug,
/// {
///     type Arg = __Wit;
/// }
/// impl<'a, T> typewit::MakeTypeWitness for Witness<'a, T, T>
/// where
///     T: Debug,
///     T: Copy,
/// {
///     const MAKE: Self = Self::Value(typewit::TypeEq::NEW);
/// }
/// impl<'a, T> typewit::MakeTypeWitness for Witness<'a, T, &'a T>
/// where
///     T: Debug,
/// {
///     const MAKE: Self = Self::Ref(typewit::TypeEq::NEW);
/// }
/// ```
/// 
/// <span id = "var_gen_args-example"></span>
/// ### `$var_gen_args` parameter
/// 
/// This example shows what the `$var_gen_args` parameter does.
/// 
/// ```rust
/// typewit::simple_type_witness! {
///     // Generic parameters go inside square brackets (these are optional),
///     // the macro implicitly adds a `__Wit` type parameter after all generics.
///     enum Foo[T, const N: usize] {
///         // The `[(), 0]` here
///         // replaces `impl<T, const N: usize> MakeTypeWitness for Foo<T, N, u64>`
///         // with     `impl MakeTypeWitness for Foo<(), 0, u64>`.
///         // 
///         // Doing this allows the  `T` and `N` type arguments to be inferred
///         // every time that this variant is constructed.
///         U64[(), 0] = u64,
///         Array = [T; N],
///     }
/// }
/// ```
/// the above effectively expands to this:
/// ```rust
/// enum Foo<T, const N: usize, __Wit> {
///     U64(typewit::TypeEq<__Wit, u64>),
///     Array(typewit::TypeEq<__Wit, [T; N]>),
/// }
/// impl<T, const N: usize, __Wit> typewit::TypeWitnessTypeArg for Foo<T, N, __Wit> {
///     type Arg = __Wit;
/// }
/// impl typewit::MakeTypeWitness for Foo<(), 0, u64> {
///     const MAKE: Self = Self::U64(typewit::TypeEq::NEW);
/// }
/// impl<T, const N: usize> typewit::MakeTypeWitness for Foo<T, N, [T; N]> {
///     const MAKE: Self = Self::Array(typewit::TypeEq::NEW);
/// }
/// ```
/// (consult the [generated items] section for all the generated impls)
/// 
/// [`TypeEq`]: crate::TypeEq
/// [`TypeWitnessTypeArg`]: crate::TypeWitnessTypeArg
/// [`MakeTypeWitness`]: crate::MakeTypeWitness
/// [generated items]: #generated-items
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
                = $witnessed_ty:ty
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
                        = $witnessed_ty
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
                = $witnessed_ty:ty
            ))*
        }
    ) => {
        $(#$enum_meta)*
        $vis enum $enum <$($generics)* __Wit> 
        where $($where)*
        {
            $(
                $(#[$variant_meta])*
                $variant($crate::TypeEq<__Wit, $witnessed_ty>),
            )*
        }

        impl<$($generics)* __Wit> $crate::__::Copy for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {}

        impl<$($generics)* __Wit> $crate::__::Clone for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {
            fn clone(&self) -> Self {
                *self
            }
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
            = $witnessed_ty:ty
        )
    ) => {
        impl<$($generics)*> $crate::MakeTypeWitness for $enum<$($gen_args)* $witnessed_ty>
        where
            $(
                $enum<$($var_gen_args,)* $witnessed_ty>:
                    $crate::__::TypeIdentity<Type = Self>,
            )?
            $($where)*
            $($vari_where)*
        {
            const MAKE: Self = Self::$variant($crate::TypeEq::NEW);
        }

    }
}




