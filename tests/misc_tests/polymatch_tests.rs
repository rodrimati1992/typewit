use typewit::polymatch;

#[derive(Copy, Clone)]
enum FourInts {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}


#[test]
fn test_nexted_or_patterns() {
    struct Foo;

    for (int, expected) in [
        (2, false), 
        (3, true), 
        (4, false), 
        (5, true), 
        (6, false),
        (8, true),
    ] {
        let actual = polymatch!(int;
            (3 | 5 | 8) => { 
                impl Foo {
                    fn bar() -> u32 { 0x600D }
                }

                true
            }
            _ => false
        );

        assert_eq!(expected, actual, "int: {int}");
    }

    assert_eq!(Foo::bar(), 0x600D);
}

#[test]
fn test_one_syntactic_branch() {
    macro_rules! test_case {
        ( ($sum:ident $x:ident) ($($or:tt)?) ($($expr:tt)*)) => {{
            let mut $sum = 0u64;
            
            for var in [
                FourInts::U8(1),
                FourInts::U16(4),
                FourInts::U32(16),
                FourInts::U64(64),
            ] {
                polymatch!{var;
                    $($or)? FourInts::U8($x) | FourInts::U16($x) | 
                    FourInts::U32($x) | FourInts::U64($x) => $($expr)*
                }
            }

            assert_eq!($sum, 85u64);        
        }};
    }

    // no braces
    test_case!{(sum x) ( ) (sum += u64::from(x))}
    test_case!{(sum x) (|) (sum += u64::from(x))}

    test_case!{(sum x) ( ) (sum += u64::from(x),)}
    test_case!{(sum x) (|) (sum += u64::from(x),)}


    // braces
    test_case!{(sum x) ( ) ({sum += u64::from(x)})}
    test_case!{(sum x) (|) ({sum += u64::from(x)})}

    test_case!{(sum x) ( ) ({sum += u64::from(x)},)}
    test_case!{(sum x) (|) ({sum += u64::from(x)},)}
}





#[test]
fn test_two_syntactic_branches() {
    macro_rules! test_case {
        ( ($sum:ident $x:ident) ($($or:tt)?) ($($expr_one:tt)*) ($($expr_two:tt)*)) => {{
            let mut $sum = 0u64;
            
            for var in [
                FourInts::U8(1),
                FourInts::U16(4),
                FourInts::U32(16),
                FourInts::U64(64),
            ] {
                polymatch!{var;
                    $($or)? FourInts::U8($x) | FourInts::U16($x) => $($expr_one)*
                    $($or)? FourInts::U32($x) | FourInts::U64($x) => $($expr_two)*
                }
            }

            assert_eq!($sum, 165u64);
        }};
    }

    // different syntaxes for first branch
    test_case!{(sum x) ( ) (sum += u64::from(u16::from(x)),) (sum += 2 * u64::from(x))}
    test_case!{(sum x) (|) (sum += u64::from(u16::from(x)),) (sum += 2 * u64::from(x))}

    test_case!{(sum x) () ({sum += u64::from(u16::from(x))}) (sum += 2 * u64::from(x))}
    test_case!{(sum x) (|) ({sum += u64::from(u16::from(x))},) (sum += 2 * u64::from(x))}

    
    // different expr syntaxes for second branch
    test_case!{(sum x) () (sum += u64::from(u16::from(x)),) (sum += 2 * u64::from(x))}
    test_case!{(sum x) (|) (sum += u64::from(u16::from(x)),) (sum += 2 * u64::from(x),)}
    test_case!{(sum x) (|) (sum += u64::from(u16::from(x)),) ({sum += 2 * u64::from(x)})}
    test_case!{(sum x) () (sum += u64::from(u16::from(x)),) ({sum += 2 * u64::from(x)},)}
}


#[test]
fn test_guard() {
    macro_rules! test_case {
        (($sum:ident $x:ident) ($($expr:tt)*)) => ({
            let mut $sum = 0u64;
        
            for var in [
                FourInts::U8(2),
                FourInts::U8(3),
                FourInts::U16(4),
                FourInts::U16(5),
                FourInts::U32(16),
                FourInts::U64(64),
            ] {
                polymatch!{var;
                    FourInts::U32($x) | FourInts::U64($x) => $sum += u64::from($x),
                    FourInts::U8($x) | FourInts::U16($x) if $x % 2 == 1 => $($expr)*
                    FourInts::U8($x) | FourInts::U16($x) => $sum += u64::from($x),
                }
            }

            assert_eq!($sum, 2 + 3 * 2 + 4 + 5 * 2 + 16 + 64);
        });
    }


    test_case!{(sum x) ({ sum += u64::from(x) * 2; })}
    test_case!{(sum x) (sum += u64::from(x) * 2,)}
}

