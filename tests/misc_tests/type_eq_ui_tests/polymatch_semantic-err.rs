struct Foo;

fn main(){
    typewit::polymatch! {100;
        200 | 300 => {
            impl Foo {
                fn foo() {}
            }
        }
    };
}