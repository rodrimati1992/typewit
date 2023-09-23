//! Higher Kinded Types for [`BaseTypeWitness`]es

use crate::{TypeCmp, TypeEq, TypeNe};

use super::BaseTypeWitness;

use core::fmt::Debug;


/// The type constructor for a [`BaseTypeWitness`],
/// only implemented for [`TcTypeEq`]/[`TcTypeNe`]/[`TcTypeCmp`].
pub trait BaseTypeWitnessTc: 'static + Copy + Debug {
    /// The [`BaseTypeWitness`] type that corresponds to this type constructor.
    /// 
    /// For [`TcTypeCmp`], this is [`TypeCmp`]`<L, R>`
    /// <br>
    /// For [`TcTypeEq`], this is [`TypeEq`]`<L, R>`
    /// <br>
    /// For [`TcTypeNe`], this is [`TypeNe`]`<L, R>`
    /// 
    type Type<L: ?Sized, R: ?Sized>: BaseTypeWitness<L = L, R = R, TypeCtor = Self>;
}


/// Queries the [`BaseTypeWitness`] of a [`BaseTypeWitnessTc`]
pub type TcToBaseTypeWitness<TC, L, R> = <TC as BaseTypeWitnessTc>::Type::<L, R>;


/////////////////////////////////////////////////////////////////////////////


/// The [*type constructor*](BaseTypeWitnessTc) for [`TypeCmp`].
#[derive(Debug, Copy, Clone)]
pub struct TcTypeCmp;

impl BaseTypeWitnessTc for TcTypeCmp {
    type Type<L: ?Sized, R: ?Sized> = TypeCmp<L, R>;
}


/// The [*type constructor*](BaseTypeWitnessTc) for [`TypeEq`].
#[derive(Debug, Copy, Clone)]
pub struct TcTypeEq;

impl BaseTypeWitnessTc for TcTypeEq {
    type Type<L: ?Sized, R: ?Sized> = TypeEq<L, R>;
}


/// The [*type constructor*](BaseTypeWitnessTc) for [`TypeNe`].
#[derive(Debug, Copy, Clone)]
pub struct TcTypeNe;

impl BaseTypeWitnessTc for TcTypeNe {
    type Type<L: ?Sized, R: ?Sized> = TypeNe<L, R>;
}
