use typewit::{
    methods::{zip2, zip3, zip4},
    TypeCmp,
    TypeEq,
    TypeNe,
    type_eq,
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


#[test]
fn test_zip4() {
    const fn with<A, B, C, D, E>(eq: TypeEq<A, B>, ne: TypeNe<B, C>, cmp: TypeCmp<D, E>) {
        let _: TypeEq<(A, u64, B, i64), (B, u64, A, i64)> = 
            zip4(eq, type_eq(), eq.flip(), type_eq());
        let _: TypeNe<(A, E, B, B), (B, D, A, C)> = zip4(eq, cmp.flip(), eq.flip(), ne);
        let _: TypeCmp<(D, A, B, A), (E, B, A, B)> = zip4(cmp, eq, eq.flip(), eq);
    }

    with::<u8, u8, bool, u16, u32>(TypeEq::NEW, TypeNe::with_any().unwrap(), TypeCmp::with_any());
}

#[test]
fn test_in_array() {
    use typewit::{
        methods::in_array,
        const_marker::Usize,
    };

    let eq_ty: TypeEq<i16, i16> = TypeEq::NEW;
    let ne_ty: TypeNe<i16, u16> = TypeNe::with_any().unwrap();
    let cmp_ty: TypeCmp<i16, u16> = TypeCmp::with_any();
    
    let eq_len: TypeEq<Usize<0>, Usize<0>> = TypeEq::NEW;
    let ne_len: TypeNe<Usize<1>, Usize<2>> = Usize.equals(Usize).unwrap_ne();
    let cmp_len: TypeCmp<Usize<3>, Usize<3>> = Usize.equals(Usize);
    
    with(eq_ty, ne_ty, cmp_ty, eq_len, ne_len, cmp_len);

    const fn with<A, B, const J: usize, const K: usize, const L: usize, const M: usize>(
        eq_ty: TypeEq<A, A>,
        ne_ty: TypeNe<A, B>,
        cmp_ty: TypeCmp<A, B>,
        eq_len: TypeEq<Usize<J>, Usize<J>>,
        ne_len: TypeNe<Usize<K>, Usize<L>>,
        cmp_len: TypeCmp<Usize<M>, Usize<M>>,
    ) {
        // if both arguments are TypeEq, this returns a TypeEq
        let _: TypeEq<[A; J], [A; J]> = in_array(eq_ty, eq_len);
        
        // if either of the arguments is a TypeNe, this returns a TypeNe
        let _: TypeNe<[A; J], [B; J]> = in_array(ne_ty, eq_len);
        let _: TypeNe<[A; K], [A; L]> = in_array(eq_ty, ne_len);
        let _: TypeNe<[A; K], [B; L]> = in_array(ne_ty, ne_len);
        let _: TypeNe<[A; K], [B; L]> = in_array(cmp_ty, ne_len);
        let _: TypeNe<[A; M], [B; M]> = in_array(ne_ty, cmp_len);
        
        // If there are TypeCmp args, and no TypeNe args, this returns a TypeCmp
        let _: TypeCmp<[A; M], [A; M]> = in_array(eq_ty, cmp_len);
        let _: TypeCmp<[A; J], [B; J]> = in_array(cmp_ty, eq_len);
        let _: TypeCmp<[A; M], [B; M]> = in_array(cmp_ty, cmp_len);        
    }
    

}



