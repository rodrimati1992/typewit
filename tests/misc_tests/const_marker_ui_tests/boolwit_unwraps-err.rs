use typewit::const_marker::{Bool, BoolWitG};
use typewit::TypeEq;


const T: BoolWitG<Bool<true>> = BoolWitG::True(TypeEq::NEW);
const F: BoolWitG<Bool<false>> = BoolWitG::False(TypeEq::NEW);

const _: TypeEq<Bool<true>, Bool<false>> = T.unwrap_false();

const _: TypeEq<Bool<true>, Bool<false>> = T.expect_false("testing testing 123");

const _: TypeEq<Bool<false>, Bool<true>> = F.unwrap_true();

const _: TypeEq<Bool<false>, Bool<true>> = F.expect_true("testing testing 456");


fn main() {}