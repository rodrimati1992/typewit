use typewit::MakeTypeWitness;

typewit::simple_type_witness!{
    enum LifetimeBounds['a, 'b: 'a, 'c: 'b + 'a, 'd: 'a + 'static] {
        U8 = (&'a (), &'b (), &'c (), &'d ()) 
    }
}

typewit::simple_type_witness!{
    enum LifetimeBoundsAngBrack<'a, 'b: 'a, 'c: 'b + 'a, 'd: 'a + 'static> {
        U8 = (&'a (), &'b (), &'c (), &'d ()) 
    }
}

fn _foo<'a, 'b, 'c, 'd>() {
    let _: LifetimeBounds<'a, 'b, 'c, 'd, _> = MakeTypeWitness::MAKE;
}

fn _bar<'a, 'b, 'c, 'd>() {
    let _: LifetimeBoundsAngBrack<'a, 'b, 'c, 'd, _> = MakeTypeWitness::MAKE;
}

fn main(){}