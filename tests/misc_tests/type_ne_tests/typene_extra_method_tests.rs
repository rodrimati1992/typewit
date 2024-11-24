use typewit::{
    type_fn::{GRef, GRefMut, TypeFn},
    TypeNe,
    type_ne,
};

use crate::misc_tests::test_utils::{assert_type, assert_type_eq};

#[test]
fn map_test() {
    assert_type::<_, TypeNe<&u8, &u16>>(type_ne!(u8, u16).map(GRef::NEW));
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(type_ne!(u8, u16).map(GRefMut::NEW));
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


    assert_type::<_, TypeNe<&u8, &u16>>(type_ne!(u8, u16).project::<GRef<'_>>());
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(type_ne!(u8, u16).project::<GRefMut<'_>>());
    assert_type::<_, TypeNe<Foo<u8>, Foo<u16>>>(type_ne!(u8, u16).project::<FooFn>());
}


#[test]
fn unmap_test() {
    {
        let ne: TypeNe<&u8, &u16> = type_ne!(u8, u16).map(GRef::NEW);
        assert_type_eq(ne.unmap(GRef::NEW),  type_ne!(u8, u16));
    }
    {
        let ne: TypeNe<&mut u8, &mut u16> = type_ne!(u8, u16).map(GRefMut::NEW);
        assert_type_eq(ne.unmap(GRefMut::NEW),  type_ne!(u8, u16));
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
        let ne: TypeNe<&u8, &u16> = type_ne!(u8, u16).project::<GRef<'_>>();
        assert_type_eq(ne.unproject::<GRef<'_>>(),  type_ne!(u8, u16));
    }
    {
        let ne: TypeNe<&mut u8, &mut u16> = type_ne!(u8, u16).project::<GRefMut<'_>>();
        assert_type_eq(ne.unproject::<GRefMut<'_>>(),  type_ne!(u8, u16));
    }
    assert_eq!(type_ne!(Foo<u8>, Foo<u16>).unproject::<FooFn>(), type_ne!(u8, u16));
}




#[test]
fn in_ref_test() {
    assert_type::<_, TypeNe<&u8, &u16>>(type_ne!(u8, u16).in_ref());
}

#[test]
fn in_mut_test() {
    assert_type::<_, TypeNe<&mut u8, &mut u16>>(type_ne!(u8, u16).in_mut());

    #[cfg(feature = "rust_1_83")]
    {
        const fn _in_mut<T>(te: TypeNe<T, u8>) {
            let _: TypeNe<&mut T, &mut u8> = te.in_mut();
        }
    }
}

#[cfg(feature = "alloc")]
#[test]
fn in_box_test() {    
    assert_type::<_, TypeNe<Box<u8>, Box<u16>>>(type_ne!(u8, u16).in_box());
}


#[cfg(feature = "rust_1_61")]
#[test]
fn test_in_array() {
    use typewit::const_marker::Usize;

    {
        let ne = type_ne!(u8, u16).in_array(Usize::<1>.equals(Usize::<2>));
        assert_type::<_, TypeNe<[u8; 1], [u16; 2]>>(ne);
    }
    {
        let ne = type_ne!(u8, u16).in_array(Usize::<1>.equals(Usize::<2>).unwrap_ne());
        assert_type::<_, TypeNe<[u8; 1], [u16; 2]>>(ne);
    }

    {
        let ne = type_ne!(u8, u16).in_array(Usize::<1>.equals(Usize::<1>).unwrap_eq());
        assert_type::<_, TypeNe<[u8; 1], [u16; 1]>>(ne);
    }
    {
        let ne = type_ne!(u8, u16).in_array(Usize::<1>.equals(Usize::<1>));
        assert_type::<_, TypeNe<[u8; 1], [u16; 1]>>(ne);
    }
}
