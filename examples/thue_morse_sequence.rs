extern crate rdd;

use rdd::bdd::{BDDArena, BooleanProgrammingSolver};

fn get_weight(x: usize) -> i64 {
    if x.count_ones() % 2 == 0 {
        1
    } else {
        -1
    }
}

fn main() -> std::io::Result<()> {
    // Example of linear boolean programming.
    // Optimize kernel of cycle graph.
    // Weight of each node is defined by Thue-Morse sequence.

    const N: usize = 33;
    let mut arena = BDDArena::new();
    let mut vars = vec![];
    for i in 0..N {
        vars.push(arena.new_var(format!("n{}", i + 1)));
    }

    // Define kernel of cycle graph in BDD.
    let mut f = arena.false_bdd();
    for i in 0..N {
        f |= vars[i] & vars[(i + 1) % N];
    }
    f = !f;
    for i in 0..N {
        f &= vars[(N + i - 1) % N] | vars[i] | vars[(i + 1) % N];
    }

    let f = f.serialize();
    println!("The number of nodes in bdd: {}", f.num_nodes());

    // Define weight of linear boolean programming: Thue-Morse sequence.
    let mut weight = vec![];
    for i in 0..N {
        weight.push(get_weight(i + 1));
    }

    // Solve boolean programming.
    let solver = BooleanProgrammingSolver::new(f, weight);
    let (opt_value, opt) = solver.solve();

    println!("Cycle graph: 1 - 2 - 3 - ... - {} - 1", N);
    println!("OPT: {}", opt_value);
    print!("OPT SET: ");
    for i in opt.into_iter() {
        let weight = get_weight((i.x() + 1) as usize);
        if weight < 0 {
            print!("**{}** ", i.x() + 1);
        } else {
            print!("{} ", i.x() + 1);
        }
    }
    println!("");

    Ok(())
}
