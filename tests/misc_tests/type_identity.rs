use typewit::{Identity, TypeEq};


fn str_bounds<L: ?Sized, R: ?Sized>() -> (&'static L, &'static R)
where
    L: Identity<Type = str>,
    str: Identity<Type = R>,
{
    let tel: TypeEq<L, str> = Identity::TYPE_EQ;
    let ter: TypeEq<str, R> = Identity::TYPE_EQ;

    (tel.in_ref().to_left("foo"), ter.in_ref().to_right("bar"))
}

#[test]
fn unsized_type_identity() {
    assert_eq!(str_bounds::<str, _>(), ("foo", "bar"));
}

fn sized_bound<L, R>() -> (L, R)
where
    L: Identity<Type = u8>,
    u8: Identity<Type = R>,
{
    let tel: TypeEq<L, u8> = Identity::TYPE_EQ;
    let ter: TypeEq<u8, R> = Identity::TYPE_EQ;

    (tel.to_left(3u8), ter.to_right(5u8))
}

#[test]
fn u8_type_identity() {
    assert_eq!(sized_bound::<u8, _>(), (3, 5));
}