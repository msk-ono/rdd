use rand::thread_rng;
use rdd::bdd::{dump_dot, sample_answers_from_bdd, DumpDotOption};
use rdd::parser::parse;

fn pretty_error(input: &str, start: usize, end: usize) -> String {
    let mut ret = String::new();
    ret += input;
    ret += &vec![' '; start - 1].into_iter().collect::<String>();
    ret += &vec!['^'; end - start + 2].into_iter().collect::<String>();
    ret
}

fn main() {
    const NUM_SAMPLES: usize = 5;
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
        let (arena, bdd) = result.unwrap();
        println!("Num answers: {}", bdd.num_answers());
        println!("Num nodes: {}", bdd.num_nodes());
        println!("Sample answers:");
        let sbdd = bdd.serialize();
        let mut rng = thread_rng();
        let samples = sample_answers_from_bdd(&sbdd, &mut rng, NUM_SAMPLES);
        for sample in samples.iter() {
            for (var, value) in sample.into_iter() {
                print!("{}: {}, ", arena.name(*var), value);
            }
            println!();
        }
        println!(
            "Graphviz:\n{}",
            dump_dot(&arena, &sbdd, "BDD", &DumpDotOption::default())
        );
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
