use typewit::{
    prim_type_wit::{zip2, zip3},
    TypeCmp,
    TypeEq,
    TypeNe,
};

use crate::misc_tests::test_utils::assertm;


#[test]
fn zip2_test() {
    const fn constness<A, B, C, D, E, F>(
        eq: TypeEq<A, B>,
        ne: TypeNe<B, C>, 
        cmp_eq: TypeCmp<D, E>,
        cmp_ne: TypeCmp<E, F>,
    ) {
        let _: TypeEq<(A, B), (B, A)> = zip2(eq, eq.flip());
        let _: TypeNe<(A, B), (B, C)> = zip2(eq, ne);
        {
            let x: TypeCmp<(A, D), (B, E)> = zip2(eq, cmp_eq);
            assertm!(x, TypeCmp::Eq{..});
        }
        {
            let x: TypeCmp<(A, E), (B, F)> = zip2(eq, cmp_ne);
            assertm!(x, TypeCmp::Ne{..});
        }

        let _: TypeNe<(B, A), (C, B)> = zip2(ne, eq);
        let _: TypeNe<(B, C), (C, B)> = zip2(ne, ne.flip());
        let _: TypeNe<(B, D), (C, E)> = zip2(ne, cmp_eq);
        let _: TypeNe<(B, E), (C, F)> = zip2(ne, cmp_ne);

        {
            let x: TypeCmp<(D, A), (E, B)> = zip2(cmp_eq, eq);
            assertm!(x, TypeCmp::Eq{..});
        }
        let _: TypeNe<(D, B), (E, C)> = zip2(cmp_eq, ne);
        {
            let x: TypeCmp<(D, E), (E, D)> = zip2(cmp_eq, cmp_eq.flip());
            assertm!(x, TypeCmp::Eq{..});
        }
        {
            let x: TypeCmp<(D, E), (E, F)> = zip2(cmp_eq, cmp_ne);
            assertm!(x, TypeCmp::Ne{..});
        }
    }

    constness::<u8, u8, bool, u32, u32, u64>(
        TypeEq::NEW,
        TypeNe::with_any().unwrap(),
        TypeCmp::with_any(),
        TypeCmp::with_any(),
    );
}


#[test]
fn zip3_test() {
    const fn constness<A, B, C, D, E, F, G>(
        eq0: TypeEq<A, B>,
        eq1: TypeEq<B, C>,
        ne: TypeNe<C, D>, 
        cmp_eq: TypeCmp<E, F>,
        cmp_ne: TypeCmp<F, G>,
    ) {
        let _: TypeEq<(A, B, B), (B, C, A)> = zip3(eq0, eq1, eq0.flip());

        let _: TypeNe<(C, A, B), (D, B, C)> = zip3(ne, eq0, eq1);
        let _: TypeNe<(A, C, B), (B, D, A)> = zip3(eq0, ne, eq0.flip());
        let _: TypeNe<(A, B, C), (B, C, D)> = zip3(eq0, eq1, ne);

        {
            let x = zip3(cmp_eq, eq0, eq1);
            assertm!(x, TypeCmp::<(E, A, B), (F, B, C)>::Eq(_));
        }
        {
            let x = zip3(eq0, cmp_eq, eq0.flip());
            assertm!(x, TypeCmp::<(A, E, B), (B, F, A)>::Eq(_));
        }
        {
            let x = zip3(eq0, eq1, cmp_eq);
            assertm!(x, TypeCmp::<(A, B, E), (B, C, F)>::Eq(_));
        }
        {
            let x = zip3(cmp_ne, eq0, eq1);
            assertm!(x, TypeCmp::<(F, A, B), (G, B, C)>::Ne(_));
        }
        {
            let x = zip3(eq0, cmp_ne, eq0.flip());
            assertm!(x, TypeCmp::<(A, F, B), (B, G, A)>::Ne(_));
        }
        {
            let x = zip3(eq0, eq1, cmp_ne);
            assertm!(x, TypeCmp::<(A, B, F), (B, C, G)>::Ne(_));
        }
    }

    constness::<u8, u8, u8, bool, u32, u32, u64>(
        TypeEq::NEW,
        TypeEq::NEW,
        TypeNe::with_any().unwrap(),
        TypeCmp::with_any(),
        TypeCmp::with_any(),
    );
}


