use typewit::{CallFn, CallInjFn, InjTypeFn, RevTypeFn, TypeFn, UncallFn};

use crate::misc_tests::test_utils::{AssertEq, assert_type_eq};

use core::marker::PhantomData as PD;


// have to implement this type-level function manually,
// because the TypeFn and RevTypeFn impls need different bounds.
struct UpcastPair;

impl<T, U> TypeFn<(T, U)> for UpcastPair 
where
    Upcast: TypeFn<T> + TypeFn<U>,
    CallFn<Upcast, T>: Sized, 
    CallFn<Upcast, U>: Sized,
{
    type Output = (CallFn<Upcast, T>, CallFn<Upcast, U>);
}

impl<TR, UR> RevTypeFn<(TR, UR)> for UpcastPair 
where
    Upcast: RevTypeFn<TR> + RevTypeFn<UR>,
    UncallFn<Upcast, TR>: Sized, 
    UncallFn<Upcast, UR>: Sized,
{
    type Arg = (UncallFn<Upcast, TR>, UncallFn<Upcast, UR>);
}


typewit::inj_type_fn! {
    struct Upcast;

    impl u8 => u16;
    impl u16 => u32;
    impl u32 => u64;
    impl u64 => u128;
}

#[test]
fn test_injtypefn_basic() {
    let _: AssertEq<CallFn<Upcast, u8>, u16>;
    let _: AssertEq<CallFn<Upcast, u16>, u32>;
    let _: AssertEq<CallFn<Upcast, u32>, u64>;
    let _: AssertEq<CallFn<Upcast, u64>, u128>;
    
    let _: AssertEq<CallInjFn<Upcast, u8>, u16>;
    let _: AssertEq<CallInjFn<Upcast, u16>, u32>;
    let _: AssertEq<CallInjFn<Upcast, u32>, u64>;
    let _: AssertEq<CallInjFn<Upcast, u64>, u128>;
    
    let _: AssertEq<UncallFn<Upcast, u16>, u8>;
    let _: AssertEq<UncallFn<Upcast, u32>, u16>;
    let _: AssertEq<UncallFn<Upcast, u64>, u32>;
    let _: AssertEq<UncallFn<Upcast, u128>, u64>;
}


#[test]
fn test_injtypefn_arg_inference() {
    #[track_caller]
    fn assert_arg_type<F, A, E>() -> PD<CallInjFn<F, A>>
    where
        F: InjTypeFn<A>
    {
        assert_type_eq::<PD<A>, PD<E>>(PD, PD);

        PD
    }

    let _: PD<u16>  = assert_arg_type::<Upcast, _, u8>();
    let _: PD<u32>  = assert_arg_type::<Upcast, _, u16>();
    let _: PD<u64>  = assert_arg_type::<Upcast, _, u32>();
    let _: PD<u128> = assert_arg_type::<Upcast, _, u64>();
    
    let _: PD<(u32, u128)> = assert_arg_type::<UpcastPair, _, (u16, u64)>();
    let _: PD<(u32, u128)> = assert_arg_type::<UpcastPair, _, (u16, u64)>();
    let _: PD<(u64, u16)> = assert_arg_type::<UpcastPair, _, (u32, u8)>();
}



// only need to test cfg on impls,
// because the `type_fn` macro handles struct declaration.
typewit::inj_type_fn! {
    struct CfgAttr;

    #[cfg(all())]
    impl u8 => ();

    #[cfg(any())]
    impl what_is => going_on<these, types, dont, exist>;
}


#[test]
fn test_injtypefn_cfg() {
    let _: AssertEq<CallFn<CfgAttr, u8>, ()>;
}