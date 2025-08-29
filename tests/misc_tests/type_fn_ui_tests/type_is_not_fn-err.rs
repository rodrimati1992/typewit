typewit::type_fn!{
    struct Identity;

    impl<T> T  => T;
}

const _: () = {
    let eq: typewit::TypeEq<u8, u8> = typewit::TypeEq::NEW;
    let _ = eq.map(());
    let _ = eq.map(Identity);

    let ne: typewit::TypeNe<u8, u16> = typewit::type_ne!(u8, u16);
    let _ = ne.map(());
    let _ = ne.map(Identity);
};

fn main(){}