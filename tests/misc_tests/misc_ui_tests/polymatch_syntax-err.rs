fn main(){
    typewit::polymatch! {100
        200 | 300 => 400
    };
    typewit::polymatch! {100;
        200 | 300 => 400
        500 | 600 => 400
    };
}