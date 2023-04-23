use typewit::{
    type_fn::{GRef, GRefMut}, 
    TypeEq,
};

fn in_ref_return_different_lts<'a, 'b, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a L, &'b R> {
    te.in_ref()
}

fn in_mut_return_different_lts<'a, 'b, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a mut L, &'b mut R> {
    te.in_mut()
}

fn map_ref_return_different_lts<'a, 'b, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a L, &'b R> {
    te.map(GRef::NEW)
}

fn project_ref_return_different_lts<'a, 'b, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a L, &'b R> {
    te.project::<GRef>()
}

fn map_mut_return_different_lts<'a, 'b, L, R>(te: TypeEq<L, R>) -> TypeEq<&'a mut L, &'b mut R> {
    te.map(GRefMut::NEW)
}

fn project_mut_return_different_lts<'a, 'b, L, R>(
    te: TypeEq<L, R>
) -> TypeEq<&'a mut L, &'b mut R> {
    te.project::<GRefMut>()
}


fn main(){}