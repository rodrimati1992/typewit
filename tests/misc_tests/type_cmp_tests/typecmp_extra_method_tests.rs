use typewit::{
    type_fn::GRef,
    TypeCmp,
};

use crate::misc_tests::test_utils::assert_type;

struct Foo<T: ?Sized>(T);

typewit::inj_type_fn! {
    struct FooFn; 
    impl<T: ?Sized> T => Foo<T>
}


struct Bar<T: ?Sized>(T);

typewit::inj_type_fn! {
    struct BarFn; 
    impl<T: ?Sized> T => Bar<T>
}


#[test]
fn test_map() {
    const fn constness<L: ?Sized, R: ?Sized>(cmp: TypeCmp<L, R>) -> TypeCmp<L, R> {
        let _ = cmp.map(GRef::NEW);
        cmp
    }

    {
        let x = constness(TypeCmp::<u8, u8>::with_any()).map(GRef::NEW);
        assert_type::<_, TypeCmp<&u8, &u8>>(x);
    }
    {
        let x = constness(TypeCmp::<u8, i8>::with_any()).map(GRef::NEW);
        assert_type::<_, TypeCmp<&u8, &i8>>(x);
    }
}

#[test]
fn test_project() {
    const fn constness<L: ?Sized, R: ?Sized>(cmp: TypeCmp<L, R>) -> TypeCmp<L, R> {
        let _ = cmp.project::<FooFn>();
        cmp
    }

    {
        let x = constness(TypeCmp::<u8, u8>::with_any()).project::<FooFn>();
        assert_type::<_, TypeCmp<Foo<u8>, Foo<u8>>>(x);
    }
    {
        let x = constness(TypeCmp::<u8, i8>::with_any()).project::<FooFn>();
        assert_type::<_, TypeCmp<Foo<u8>, Foo<i8>>>(x);
    }
}

#[test]
fn test_unmap() {
    const fn constness<L: ?Sized, R: ?Sized>(
        cmp: TypeCmp<Bar<L>, Bar<R>>,
    ) -> TypeCmp<Bar<L>, Bar<R>> {
        let _ = cmp.unmap(BarFn);
        cmp
    }

    {
        let x = constness(TypeCmp::<Bar<u8>, Bar<u8>>::with_any()).unmap(BarFn);
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = constness(TypeCmp::<Bar<u8>, Bar<i8>>::with_any()).unmap(BarFn);
        assert_type::<_, TypeCmp<u8, i8>>(x);
    }
}

#[test]
fn test_unproject() {
    const fn constness<'a, L: ?Sized, R: ?Sized>(
        cmp: TypeCmp<&'a L, &'a R>,
    ) -> TypeCmp<&'a L, &'a R> {
        let _ = cmp.unproject::<GRef<'a>>();
        cmp
    }

    {
        let x = constness(TypeCmp::<&u8, &u8>::with_any()).unproject::<GRef<'_>>();
        assert_type::<_, TypeCmp<u8, u8>>(x);
    }
    {
        let x = constness(TypeCmp::<&u8, &i8>::with_any()).unproject::<GRef<'_>>();
        assert_type::<_, TypeCmp<u8, i8>>(x);
    }
}

#[test]
fn test_in_ref() {
    const fn constness<L: ?Sized, R: ?Sized>(cmp: TypeCmp<L, R>) -> TypeCmp<L, R> {
        let _ = cmp.in_ref();
        cmp
    }

    {
        let x = constness(TypeCmp::<u8, u8>::with_any()).in_ref();
        assert_type::<_, TypeCmp<&u8, &u8>>(x);
    }
    {
        let x = constness(TypeCmp::<u8, i8>::with_any()).in_ref();
        assert_type::<_, TypeCmp<&u8, &i8>>(x);
    }
}


#[test]
fn test_in_mut() {
    #[cfg(feature = "mut_refs")]
    const fn constness<L: ?Sized, R: ?Sized>(cmp: TypeCmp<L, R>) {
        let _ = cmp.in_mut();
    }

    {
        let x = TypeCmp::<u8, u8>::with_any().in_mut();
        assert_type::<_, TypeCmp<&mut u8, &mut u8>>(x);
    }
    {
        let x = TypeCmp::<u8, i8>::with_any().in_mut();
        assert_type::<_, TypeCmp<&mut u8, &mut i8>>(x);
    }
}

#[cfg(feature = "alloc")]
#[test]
fn test_in_box() {
    const fn constness<L: ?Sized, R: ?Sized>(cmp: TypeCmp<L, R>) -> TypeCmp<L, R> {
        let _ = cmp.in_box();
        cmp
    }

    {
        let x = constness(TypeCmp::<u8, u8>::with_any()).in_box();
        assert_type::<_, TypeCmp<Box<u8>, Box<u8>>>(x);
    }
    {
        let x = constness(TypeCmp::<u8, i8>::with_any()).in_box();
        assert_type::<_, TypeCmp<Box<u8>, Box<i8>>>(x);
    }
}