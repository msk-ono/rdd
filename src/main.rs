use rdd::parser::parse;

fn pretty_error(input: &str, start: usize, end: usize) -> String {
    let mut ret = String::new();
    ret += input;
    ret += &vec![' '; start - 1].into_iter().collect::<String>();
    ret += &vec!['^'; end - start + 2].into_iter().collect::<String>();
    ret
}

fn main() {
    use std::io::{self, BufRead};
    let stdio = io::stdin();
    let mut input = stdio.lock();
    let mut exp = String::new();
    let mut line = String::new();
    while input.read_line(&mut line).unwrap() > 0 {
        exp += &line;
        line.clear();
    }

    let result = parse(&exp);
    if result.is_ok() {
        let (_arena, bdd) = result.unwrap();
        println!("answers: {}", bdd.num_answers());
        // println!("graphviz: \n{}", dump_graphviz(bdd, "bdd", false));
    } else {
        let error = result.err().unwrap();
        let loc = error.loc();
        eprintln!("{}", error.name());
        if loc.is_some() {
            let loc = loc.unwrap();
            eprintln!("{}", pretty_error(&exp, loc.0, loc.1));
        }
    }
}
