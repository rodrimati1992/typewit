/// Declares a [type witness](crate#what-are-type-witnesses) enum.
/// 
#[doc = explain_type_witness!()]
/// 
/// [**examples below**](#examples)
/// 
/// # Generated items
/// 
/// This macro always generates:
/// 
/// - An enum with tuple variants, each of which has a single [`TypeEq`] field.
/// 
/// - Impls of [`Copy`] and [`Clone`] for the enum.
/// 
/// - An impl of [`TypeWitnessTypeArg`] for the enum.
/// 
/// - An impl of [`MakeTypeWitness`](crate::MakeTypeWitness) for each variant of the enum.
/// 
/// ### Derivation
/// 
/// These impls are generated if you opt into them with the [`derive(...)`](#derive) syntax:
/// 
/// - `Debug`
/// - `PartialEq`
/// - `Eq`
/// - `PartialOrd`
/// - `Ord`
/// - `Hash`
/// 
/// This macro always implements `Copy` and `Clone` for the declared type witness,
/// `derive(Copy, Clone)` does nothing.
/// 
/// # Syntax
/// 
/// This macro uses square brackets to make parsing generics and where clauses easier.
/// 
/// This macro takes an enum-like syntax:
/// ```text
///     $(#[$enum_meta:meta])*
///     // Allows deriving some traits without the bounds that 
///     // standard derives add to type parameters.
///     $(derive($($derive:ident),* $(,)?)))?
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
/// `[$($generics:tt)*]`(optional parameter): these are the generic parameters of the enum,
/// to which this macro adds an implicit `__Wit` type parameter that the variants constrain.
/// 
/// `[$($var_gen_args:tt)*]`(optional parameter)[(example usage)](#var_gen_args-example): 
/// this parameter overrides the generic arguments of the enum in its 
/// [`MakeTypeWitness`] implementation.
/// 
/// <span id = "derive"></span>
/// `derive($($derive:ident),* $(,)?)`(optional parameter)[(example)](#derive-example):
/// supports deriving the traits listed in the [derivation](#derivation) section
/// 
/// ### Limitations
/// 
/// This macro cannot parse defaulted generic parameters,
/// doing so would either require procedural macros or 
/// a very complex `macro_rules!` macro.
/// 
/// <span id = "const-parameter-limitation"></span>
/// When used in Rust versions prior to 1.59.0,
/// type witnesses declared with this macro cannot have const parameters,
/// because this macro always adds a `__Wit` type parameter after all generic parameters,
/// and those old versions don't allow type parameters after const parameters.
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
///     // Declares an `enum Witness<'a, __Wit>`,
///     // the `__Wit` type parameter is added after all generics.
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
///     // Declares an `enum Witness<'a, T, __Wit>`,
///     // the `__Wit` type parameter is added after all generics.
///     enum Witness['a, T: 'a] 
///     // the constraints in the where clause go inside square brackets 
///     where[T: 'a + Debug] 
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
///     T: 'a + Debug,
/// {
///     Value(typewit::TypeEq<__Wit, T>),
///     Ref(typewit::TypeEq<__Wit, &'a T>),
/// }
/// impl<'a, T, __Wit> typewit::TypeWitnessTypeArg for Witness<'a, T, __Wit>
/// where
///     T: 'a + Debug,
/// {
///     type Arg = __Wit;
/// }
/// impl<'a, T> typewit::MakeTypeWitness for Witness<'a, T, T>
/// where
///     T: 'a + Debug,
///     T: Copy,
/// {
///     const MAKE: Self = Self::Value(typewit::TypeEq::NEW);
/// }
/// impl<'a, T> typewit::MakeTypeWitness for Witness<'a, T, &'a T>
/// where
///     T: 'a + Debug,
/// {
///     const MAKE: Self = Self::Ref(typewit::TypeEq::NEW);
/// }
/// ```
/// (consult the [generated items] section for all the generated impls)
/// 
/// <span id = "var_gen_args-example"></span>
/// ### `$var_gen_args` parameter
/// 
/// This example shows what the `$var_gen_args` parameter does.
/// ([it also requires Rust 1.59.0](#const-parameter-limitation))
/// 
#[cfg_attr(not(feature = "rust_1_61"), doc = "```ignore")]
#[cfg_attr(feature = "rust_1_61", doc = "```rust")]
/// typewit::simple_type_witness! {
///     // Generic parameters go inside square brackets (these are optional),
///     // Declares an `enum Foo<T, const N: usize, __Wit>`,
///     // the `__Wit` type parameter is added after all generics.
///     enum Foo[T, const N: usize] {
///         // This variant requires `__Wit == u64`.
///         // 
///         // The `[(), 0]` here
///         // replaces `impl<T, const N: usize> MakeTypeWitness for Foo<T, N, u64>`
///         // with     `impl MakeTypeWitness for Foo<(), 0, u64>`.
///         // Using `[(), 0]` allows the  `T` and `N` type parameters to be inferred
///         // when the `MakeTypeWitness` impl for `Foo<_, _, u64>` is used.
///         U64[(), 0] = u64,
///         // This variant requires `__Wit == [T; N]`.
///         Array = [T; N],
///     }
/// }
/// ```
/// the above effectively expands to this:
#[cfg_attr(not(feature = "rust_1_61"), doc = "```ignore")]
#[cfg_attr(feature = "rust_1_61", doc = "```rust")]
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
/// <span id = "derive-example"></span>
/// ### Derives
/// 
/// This example demonstrates derivation of all the supported traits 
/// using the `derive(...)` syntax (as opposed to the `#[derive(...)]` attribute).
/// 
/// ```rust
/// use typewit::{MakeTypeWitness, TypeEq};
/// 
/// struct NoImpls;
///
/// assert_eq!(Witness::<u8>::MAKE, Witness::<u8>::MAKE);
/// 
/// // Witness doesn't require its type parameters to impl any traits in its derives.
/// // (the standard derives require type parameters to impl the derived trait)
/// assert_eq!(Witness::<NoImpls>::MAKE, Witness::NoImp(TypeEq::NEW));
/// 
/// typewit::simple_type_witness! {
///     // Declares an `enum Witness<__Wit>`,
///     // the `__Wit` type parameter is added after all generics.
///     derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)
///     enum Witness {
///         U8 = u8,
///         NoImp = NoImpls,
///     }
/// }
/// ```
/// 
/// 
/// [`TypeEq`]: crate::TypeEq
/// [`TypeWitnessTypeArg`]: crate::TypeWitnessTypeArg
/// [`MakeTypeWitness`]: crate::MakeTypeWitness
/// [generated items]: #generated-items
#[macro_export]
macro_rules! simple_type_witness {
    (
        $(#[$enum_meta:meta])*
        $(derive($($derive:ident),* $(,)?))?
        $(pub $(($($pub:tt)*))?)? 
        enum $enum:ident $([$($generics:tt)*])? 
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
        $crate::__parse_generics!{
            (
                $crate::__stw_with_parsed_generics!(
                    $(#[$enum_meta])*
                    derive($($($derive)*)?)
                    $(pub $(($($pub)*))? )? 
                    enum $enum {
                        $((
                            $variant $([$($var_gen_args)*])?
                            $(#[$variant_meta])*
                            where[$($($vari_where)*)?]
                            = $witnessed_ty
                        ))*
                    }
                    where[$($($where)*)?]
                )

                [$($($generics)*)?]
            )

            []
            []
            [$($($generics)*)?]
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_with_parsed_generics {
    (
        $(# $enum_meta:tt)*
        derive $derive:tt
        $vis:vis enum $enum:ident $variants:tt
        where $where:tt

        $gen_args:tt
        $phantom_args:tt
        $generics:tt
    ) => {
        $crate::__trailing_comma! {
            (
                $crate::__stw_with_parsed_args ! (
                    $(# $enum_meta)*
                    derive $derive
                    $vis enum $enum $generics $gen_args
                    $variants
                )
            )
            []
            $where
        }
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_with_parsed_args {
    (
        $(# $enum_meta:tt)*
        derive $derive:tt
        $vis:vis enum $enum:ident $generics:tt $gen_args:tt
        { $($variant_args:tt)* }
        $where:tt
    ) => {
        $crate::__stw_top_items!{
            $(# $enum_meta)*
            $vis enum $enum $generics $gen_args
            where $where

            { $($variant_args)* }
        }

        $crate::__stw_derive_dispatcher!{
            derive $derive
            enum $enum $generics $gen_args
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
                $variant:ident
                $(#[$variant_meta:meta])*
                $([$($var_gen_args:tt)*])?
                where $vari_where:tt
                = $witnessed_ty:ty
            ))*
        }
    ) => {
        $(#$enum_meta)*
        $vis enum $enum <$($generics)* __Wit: ?Sized> 
        where $($where)*
        {
            $(
                $(#[$variant_meta])*
                $variant($crate::TypeEq<__Wit, $witnessed_ty>),
            )*
        }

        impl<$($generics)* __Wit: ?Sized> $crate::__::Copy for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {}

        impl<$($generics)* __Wit: ?Sized> $crate::__::Clone for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {
            fn clone(&self) -> Self {
                *self
            }
        }

        impl<$($generics)* __Wit: ?Sized> 
            $crate::TypeWitnessTypeArg 
        for $enum<$($gen_args)* __Wit> 
        where $($where)*
        {
            type Arg = __Wit;
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_derive_dispatcher {
    (
        derive($($trait:ident)*)
        enum $enum:ident $generics:tt $gen_args:tt
        where $where:tt
        $variant_args:tt
    ) => {
        $(
            $crate::__stw_derive_dispatcher_inner!{
                $trait
                $enum $generics $gen_args
                where $where
                $variant_args
            }
        )*
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_derive_dispatcher_inner {
    (
        $trait:ident $enum:ident[$($generics:tt)*] [$($gen_args:tt)*]
        where [$($where:tt)*]
        {$(($variant:ident $($rem:tt)*))*}
    ) => {
        $crate::__stw_single_derive!{
            (
                impl<$($generics)* __Wit: ?Sized> $enum<$($gen_args)* __Wit> 
                where
                    $($where)*
            )
            (
                impl<$($generics)* __Wit: ?Sized> 
                    $crate::__::$trait 
                for $enum<$($gen_args)* __Wit> 
                where
                    $($where)*
            )
            $trait
            [$($variant)*]
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_single_derive {
    ($inh_header:tt ($($impl_header:tt)*) Debug [$($variant:ident)*]) => {
        $($impl_header)*
        {
            fn fmt(&self, f: &mut $crate::__::Formatter<'_>) -> $crate::__::FmtResult {
                f.write_str(match self {
                    $(Self::$variant{..} => stringify!($variant),)*
                })
            }
        }
    };
    ($inh_header:tt ($($impl_header:tt)*) PartialEq [$($variant:ident)*]) => {
        $($impl_header)*
        {
            fn eq(&self, other: &Self) -> $crate::__::bool {
                $crate::__::discriminant(self) == $crate::__::discriminant(other)
            }
        }
    };
    (($($inh_header:tt)*) ($($impl_header:tt)*) PartialOrd [$($variant:ident)*]) => {
        $($inh_header)* {
            const fn __variant_number(&self) -> usize {
                mod number {
                    enum __Number__ {
                        $($variant,)*
                    }
                    $( pub const $variant: $crate::__::usize = __Number__::$variant as _; )*
                }

                match self {
                    $(Self::$variant{..} => number::$variant,)*
                }
            }
        }

        $($impl_header)* {
            fn partial_cmp(&self, other: &Self) -> $crate::__::Option<$crate::__::Ordering> {
                $crate::__::PartialOrd::partial_cmp(
                    &self.__variant_number(),
                    &other.__variant_number(),
                )
            }
        }
    };
    ($inh_header:tt ($($impl_header:tt)*) Ord [$($variant:ident)*]) => {
        $($impl_header)* {
            fn cmp(&self, other: &Self) -> $crate::__::Ordering {
                $crate::__::Ord::cmp(
                    &self.__variant_number(),
                    &other.__variant_number(),
                )
            }
        }
    };
    ($inh_header:tt ($($impl_header:tt)*) Eq [$($variant:ident)*]) => {
        $($impl_header)* { }
    };
    ($inh_header:tt ($($impl_header:tt)*) Hash [$($variant:ident)*]) => {
        $($impl_header)* {
            fn hash<H: $crate::__::Hasher>(&self, state: &mut H) {
                $crate::__::Hash::hash(&$crate::__::discriminant(self), state)
            }
        }
    };
    // this is always implemented
    ($inh_header:tt $impl_header:tt Copy $variants:tt) => {};
    // this is always implemented
    ($inh_header:tt $impl_header:tt Clone $variants:tt) => {};
    ($inh_header:tt $impl_header:tt $derive:ident $variants:tt) => {
        $crate::__::compile_error!{$crate::__::concat!{
            "The `simple_type_witness` macro does not support deriving `",
            $crate::__::stringify!($derive),
            "`.\n",
            "help: You could try using `#[derive(", 
            $crate::__::stringify!($derive),
            ")]`.",
        }}
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! __stw_make_type_witness_impl {
    (
        $enum:ident[$($generics:tt)*] [$($gen_args:tt)*]
        where [$($where:tt)*]

        (
            $variant:ident
            $(#[$variant_meta:meta])*
            $([$($var_gen_args:tt),* $(,)?])?
            where[$($vari_where:tt)*]
            = $witnessed_ty:ty
        )
    ) => {
        impl<$($generics)*> $crate::MakeTypeWitness for $enum<$($gen_args)* $witnessed_ty>
        where
            $(
                $enum<$($var_gen_args,)* $witnessed_ty>:
                    $crate::__::Identity<Type = Self>,
            )?
            $($where)*
            $($vari_where)*
        {
            const MAKE: Self = Self::$variant($crate::TypeEq::NEW);
        }

    }
}




