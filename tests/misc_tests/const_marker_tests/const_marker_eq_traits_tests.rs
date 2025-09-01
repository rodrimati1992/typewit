use typewit::{Identity, TypeCmp, TypeEq};
use typewit::const_marker::{
    CmEquals, ConstMarker, ConstMarkerOf, ConstMarkerEq, ConstMarkerEqOf, HasConstMarker,
    I8, I16, U8, U16,
};
 
use core::marker::PhantomData as PD;


/////////////////////////////////////////////////////////
// Making sure that it's possible to define type-level enums that impl ConstMarkerEq.

#[test]
const fn type_level_enum_test() {
    // returns Some if L == R
    assert!(typecast_tal::<_, _, First>(&Tal(3, First)).is_some());
    assert!(typecast_tal::<_, _, Second>(&Tal(5, Second)).is_some());
    assert!(typecast_tal::<_, _, Third>(&Tal(8, Third)).is_some());
     
    // returns None if L != R
    assert!(typecast_tal::<_, _, First>(&Tal(5, Second)).is_none());
    assert!(typecast_tal::<_, _, First>(&Tal(8, Third)).is_none());
     
    assert!(typecast_tal::<_, _, Second>(&Tal(3, First)).is_none());
    assert!(typecast_tal::<_, _, Second>(&Tal(8, Third)).is_none());
     
    assert!(typecast_tal::<_, _, Third>(&Tal(3, First)).is_none());
    assert!(typecast_tal::<_, _, Third>(&Tal(5, Second)).is_none());
}
 
 
const fn typecast_tal<T, L, R>(bar: &Tal<T, L>) -> Option<&Tal<T, R>>
where
    L: ConstMarkerEqOf<Order>,
    R: ConstMarkerEqOf<Order>,
{
    typewit::type_fn!{
        struct TalFn<T>;
        impl<X> X => Tal<T, X>
    }
     
    match L::Equals::<R>::VAL {
        TypeCmp::<L, R>::Eq(te) => Some(te.map(TalFn::NEW).in_ref().to_right(bar)),
        TypeCmp::<L, R>::Ne(_) => None,
    }
}
 
 
struct Tal<T, L>(T, L);
 
// Order enum and type-level enum
 
enum Order {
    First = 1,
    Second,
    Third,
}
 
impl HasConstMarker for Order {
    type Witness<T: ConstMarkerOf<Self>> = OrderWit<T>;
}
 
typewit::simple_type_witness! {
    derive(Equals)
    enum OrderWit {
        First = First,
        Second = Second,
        Third = Third,
    }
}
 
macro_rules! declare_type_level_variant {($variant:ident) => {
    struct $variant;
 
    impl ConstMarker for $variant {
        type Of = Order;
        const VAL: Self::Of = Order::$variant;
    }
 
    impl ConstMarkerEq for $variant {
        type Equals<Rhs: ConstMarkerEqOf<Self::Of>> = OrderTypeEquals<Self, Rhs>;
    }
}}
 
declare_type_level_variant!{First}
declare_type_level_variant!{Second}
declare_type_level_variant!{Third}
 
struct OrderTypeEquals<L, R>(core::marker::PhantomData<(L, R)>);
 
impl<L, R> ConstMarker for OrderTypeEquals<L, R>
where
    L: ConstMarkerEqOf<Order>,
    R: ConstMarkerEqOf<Order>,
{
    type Of = typewit::TypeCmp<L, R>;
    const VAL: Self::Of = L::CM_WITNESS.equals(R::CM_WITNESS);
}

/////////////////////////////////////////////////////////
// Making sure that it's possible to have ConstMarkerEq as associated type,
// and that its `Of` associated type can be constrained to anoter associated type.

#[test]
fn generic_constant_test() {
    // `T == U`, does the typecast
    assert!(typecast_pair::<Pair<U8<3>, U8<5>>, Pair<U8<3>, U8<5>>>(Pair(PD)).is_ok());
    assert!(typecast_pair::<Pair<I8<3>, I8<5>>, Pair<I8<3>, I8<5>>>(Pair(PD)).is_ok());
     
    // `T != U`, returns error
    assert!(typecast_pair::<Pair<U16<3>, U16<5>>, Pair<U16<3>, U16<9>>>(Pair(PD)).is_err());
    assert!(typecast_pair::<Pair<I16<3>, I16<5>>, Pair<I16<9>, I16<5>>>(Pair(PD)).is_err());
}
 
 
const fn typecast_pair<T: IsPair, U: IsPair<Of = T::Of>>(pair: T) -> Result<U, T> {
    match T::PairEquals::<U>::VAL {
        TypeCmp::<T, U>::Eq(te) => Ok(te.to_right(pair)),
        TypeCmp::<T, U>::Ne(_) => Err(pair),
    }
}
 
#[derive(Copy, Clone)]
struct Pair<T, U>(PD<(T, U)>);
 
trait IsPair: Identity<Type = Pair<Self::T, Self::U>> + Copy {
    type Of;
     
    type T: ConstMarkerEqOf<Self::Of>;
     
    type U: ConstMarkerEqOf<Self::Of>;
     
    type PairEquals<Rhs: IsPair<Of = Self::Of>>: ConstMarkerOf<TypeCmp<Self, Rhs>>;
}
 
impl<T, U> IsPair for Pair<T, U> 
where
    T: ConstMarkerEq + Copy,
    U: ConstMarkerEqOf<T::Of> + Copy,
{
    type Of = T::Of;
    type T = T;
    type U = U;
    type PairEquals<Rhs: IsPair<Of = T::Of>> = PairEquals<Self, Rhs>;
}
 
struct PairEquals<Lhs, Rhs>(core::marker::PhantomData<(Lhs, Rhs)>);
 
impl<Lhs, Rhs> ConstMarker for PairEquals<Lhs, Rhs> 
where
    Lhs: IsPair, 
    Rhs: IsPair<Of = Lhs::Of>,
{
    type Of = TypeCmp<Lhs, Rhs>;
    const VAL: Self::Of = {
        typewit::inj_type_fn! {
            struct PairFn;
            impl<T, U> (T, U) => Pair<T, U>
        }

        let eq_lhs: TypeEq<Lhs, Pair<Lhs::T, Lhs::U>> = Lhs::TYPE_EQ;
        let eq_rhs: TypeEq<Rhs, Pair<Rhs::T, Rhs::U>> = Rhs::TYPE_EQ;
     
        let cmp_t: TypeCmp<Lhs::T, Rhs::T> = CmEquals::<Lhs::T, Rhs::T>::VAL;
        let cmp_u: TypeCmp<Lhs::U, Rhs::U> = CmEquals::<Lhs::U, Rhs::U>::VAL;
     
        let cmp: TypeCmp<Pair<Lhs::T, Lhs::U>, Pair<Rhs::T, Rhs::U>> =
            cmp_t.zip(cmp_u).map(PairFn::NEW);
     
        cmp.join_left(eq_lhs).join_right(eq_rhs.flip())
    };
}
 






























