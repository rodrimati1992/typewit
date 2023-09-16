use typewit::{TypeEq, TypeNe};
use typewit::type_fn::{GRef, GRefMut, TypeFn};

use std::mem::{align_of, size_of};

use crate::misc_tests::test_utils::{assert_not_type, assert_type, assert_type_eq, assert_type_ne};

use super::typene;



#[test]
fn map_test() {
    assert_type::<_, TypeNe<&u8, &u16>>(typene::<u8, u16>().map(GRef::NEW));
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(typene::<u8, u16>().map(GRefMut::NEW));
}

#[test]
fn project_test() {
    #[derive(Debug, PartialEq)]
    struct Foo<T>(T);

    struct FooFn;
    impl<T> TypeFn<T> for FooFn {
        type Output = Foo<T>;
    }
    impl<T> typewit::RevTypeFn<Foo<T>> for FooFn {
        type Arg = T;
    }


    assert_type::<_, TypeNe<&u8, &u16>>(typene::<u8, u16>().project::<GRef<'_>>());
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(typene::<u8, u16>().project::<GRefMut<'_>>());
    assert_type::<_, TypeNe<Foo<u8>, Foo<u16>>>(typene::<u8, u16>().project::<FooFn>());
}


#[test]
fn unmap_test() {
    {
        let ne: TypeNe<&u8, &u16> = typene::<u8, u16>().map(GRef::NEW);
        assert_type_eq(ne.unmap(GRef::NEW),  typene::<u8, u16>());
    }
    {
        let ne: TypeNe<&mut u8, &mut u16> = typene::<u8, u16>().map(GRefMut::NEW);
        assert_type_eq(ne.unmap(GRefMut::NEW),  typene::<u8, u16>());
    }

}

#[test]
fn unproject_test() {
    #[derive(Debug, PartialEq)]
    struct Foo<T>(T);

    struct FooFn;
    impl<T> TypeFn<T> for FooFn {
        type Output = Foo<T>;
    }
    impl<T> typewit::RevTypeFn<Foo<T>> for FooFn {
        type Arg = T;
    }

    {
        let ne: TypeNe<&u8, &u16> = typene::<u8, u16>().project::<GRef<'_>>();
        assert_type_eq(ne.unproject::<GRef<'_>>(),  typene::<u8, u16>());
    }
    {
        let ne: TypeNe<&mut u8, &mut u16> = typene::<u8, u16>().project::<GRefMut<'_>>();
        assert_type_eq(ne.unproject::<GRefMut<'_>>(),  typene::<u8, u16>());
    }
    assert_eq!(typene::<Foo<u8>, Foo<u16>>().unproject::<FooFn>(), typene::<u8, u16>());
}
