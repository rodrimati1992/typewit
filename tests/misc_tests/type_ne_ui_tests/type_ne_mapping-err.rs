use typewit::{TypeFn, TypeNe};

use std::marker::PhantomData as PD;

struct IiFn;

impl<I: IntoIterator> TypeFn<I> for IiFn {
    type Output = I::Item;
}

struct LifetimesFoo<'a, 'b>(PD<(&'a (), &'b ())>);

impl<'a, 'b: 'a> TypeFn<&'a u32> for LifetimesFoo<'a, 'b> {
    type Output = &'b i32;
}
impl<'a, 'b: 'a> TypeFn<&'a u16> for LifetimesFoo<'a, 'b> {
    type Output = &'b i16;
}


struct LifetimesBar<'a, 'b>(PD<(&'a (), &'b ())>);

impl<'a, 'b: 'a> TypeFn<&'b u32> for LifetimesBar<'a, 'b> {
    type Output = &'a i32;
}
impl<'a, 'b: 'a> TypeFn<&'b u16> for LifetimesBar<'a, 'b> {
    type Output = &'a i16;
}



type Pair<T> = (T, T);

fn lifetime1_mismatch<'a>(ne: TypeNe<&'a str, ()>) -> Pair<TypeNe<Vec<&'static str>, Option<()>>> {
    (ne.map_to_arg(IiFn), ne.project_to_arg::<IiFn, _, _>())
}

fn lifetime2_mismatch<'b>(
    ne: TypeNe<&'b i16, &'b i32>
) -> TypeNe<&'static u16, &'static u32> {
    ne.map_to_arg(LifetimesFoo(PD))
}

fn lifetime2_rev_sensible<'b>(
    ne: TypeNe<&'static i16, &'static i32>
) -> TypeNe<&'b u16, &'b u32> {
    ne.map_to_arg(LifetimesFoo(PD))
}

fn lifetime3_sensible<'b>(
    ne: TypeNe<&'b i16, &'b i32>
) -> TypeNe<&'static u16, &'static u32> {
    ne.map_to_arg(LifetimesBar(PD))
}

fn lifetime3_rev_mismatch<'b>(
    ne: TypeNe<&'static i16, &'static i32>
) -> TypeNe<&'b u16, &'b u32> {
    ne.map_to_arg(LifetimesBar(PD))
}

fn mismatch_type<'a>(ne: TypeNe<&'a str, ()>) -> Pair<TypeNe<[u32; 2], Result<char, i8>>> {
    (ne.map_to_arg(IiFn), ne.project_to_arg::<IiFn, _, _>())
}



fn main() {}