use nom::bytes::complete::is_a;
use nom::character::complete::*;
use nom::combinator::eof;
use nom::IResult;
use nom::{branch::*, sequence::delimited};
use nom_locate::LocatedSpan;
use nom_recursive::{recursive_parser, RecursiveInfo};

// Input type must implement trait HasRecursiveInfo
// nom_locate::LocatedSpan<T, RecursiveInfo> implements it.
type Span<'a> = LocatedSpan<&'a str, RecursiveInfo>;
type Res<'a> = IResult<Span<'a>, Tree>;

enum Nonterm {
    ValueName,
}

enum Tree {
    Node(Nonterm, Vec<Tree>),
    Leaf(String),
}

use Nonterm::*;
use Tree::*;

fn token<'a, R>(p: impl Fn(Span) -> IResult<Span, R>) -> impl FnMut(Span<'a>) -> IResult<Span, R> {
    delimited(multispace0, p, alt((multispace1, eof)))
}

#[recursive_parser]
fn expr(s: Span, follow: impl Fn(Span) -> Res) -> Res {

}

fn value_name(s: Span, follow: impl Fn(Span) -> Res) -> Res {
    let (s, name) = token(is_a("abcdefghijklmnopqrstuvwxyz_"))(s)?;
    let (s, follow) = follow(s)?;
}

const PROG: &str = "
let rec short_fold f init list = match list with
  | x :: xs -> let ( cont , acc ) = f init x in
    if cont then short_fold f acc xs else acc
  | _ -> init
in
  let prod = short_fold ( fun acc x -> ( not ( x = 0 ) , acc * x ) ) 1
  in
    prod [ 3 ; 4 ; 5 ; 0 ; 6 ; 7 ]
";

fn main() {
    let ret = expr(LocatedSpan::new_extra(PROG, RecursiveInfo::new()));
    println!("{:?}", ret.unwrap().1);
}
