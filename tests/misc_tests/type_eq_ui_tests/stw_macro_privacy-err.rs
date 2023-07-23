use typewit::MakeTypeWitness;

mod private {
    typewit::simple_type_witness!{
        enum Private {
            U8 = u8
        }
    }    
}


fn main(){
    let _: private::Private<u8> = MakeTypeWitness::MAKE;
}