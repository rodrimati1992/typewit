pub(crate) mod generics_parsing;
pub(crate) mod simple_type_witness_macro;
mod type_fn_macro;

#[cfg(feature = "inj_type_fn")]
mod inj_type_fn_macro;

mod polymatch;

mod type_ne_macro;
