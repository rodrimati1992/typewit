use typewit::{
    prim_type_wit::zip2,
    TypeCmp,
    TypeEq,
    TypeNe,
};

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
            assert!(matches!(x, TypeCmp::Eq{..}));
        }
        {
            let x: TypeCmp<(A, E), (B, F)> = zip2(eq, cmp_ne);
            assert!(matches!(x, TypeCmp::Ne{..}));
        }

        let _: TypeNe<(B, A), (C, B)> = zip2(ne, eq);
        let _: TypeNe<(B, C), (C, B)> = zip2(ne, ne.flip());
        let _: TypeNe<(B, D), (C, E)> = zip2(ne, cmp_eq);
        let _: TypeNe<(B, E), (C, F)> = zip2(ne, cmp_ne);

        {
            let x: TypeCmp<(D, A), (E, B)> = zip2(cmp_eq, eq);
            assert!(matches!(x, TypeCmp::Eq{..}));
        }
        let _: TypeNe<(D, B), (E, C)> = zip2(cmp_eq, ne);
        {
            let x: TypeCmp<(D, E), (E, D)> = zip2(cmp_eq, cmp_eq.flip());
            assert!(matches!(x, TypeCmp::Eq{..}));
        }
        {
            let x: TypeCmp<(D, E), (E, F)> = zip2(cmp_eq, cmp_ne);
            assert!(matches!(x, TypeCmp::Ne{..}));
        }
    }

    constness::<u8, u8, bool, u32, u32, u64>(
        TypeEq::NEW,
        TypeNe::with_any().unwrap(),
        TypeCmp::with_any(),
        TypeCmp::with_any(),
    );
}


