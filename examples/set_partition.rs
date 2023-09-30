extern crate rdd;

use rdd::bdd::{dump_dot, BDDArena, DumpDotOption};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

// This program counts the number of tuples.
//
// Example: N=4.
// There are six tuples: (0, 1), (0, 2), (0, 3), (1, 2), (1, 3), (2, 3).
// Count the number of tuple subsets whose contents do not overlap each other.
// There are ten subsets.
// 1.  {}
// 2.  {(0, 1)}
// 3.  {(0, 2)}
// 4.  {(0, 3)}
// 5.  {(1, 2)}
// 6.  {(1, 3)}
// 7.  {(2, 3)}
// 8.  {(0, 1), (2, 4)}
// 9.  {(0, 2), (1, 3)}
// 10. {(0, 3), (1, 2)}

fn main() -> std::io::Result<()> {
    let mut arena = BDDArena::new();
    let n = 6;
    let mut var = HashMap::new();
    for i in 0..n {
        for j in i + 1..n {
            let key = (i, j);
            let v = arena.new_var(format!("X[{}][{}]", i, j));
            var.insert(key, v);
        }
    }
    let mut f = arena.true_bdd();
    for i in 0..n {
        for j in 0..n {
            for k in j + 1..n {
                if i == j || i == k {
                    continue;
                }
                let key1 = if i < j { (i, j) } else { (j, i) };
                let v1 = *var.get(&key1).unwrap();
                let key2 = if i < k { (i, k) } else { (k, i) };
                let v2 = *var.get(&key2).unwrap();
                let x = v1 & v2;
                f &= !x;
            }
        }
    }

    let num_answers = f.num_answers();
    let num_nodes = f.num_nodes();
    println!("Num answers: {}", num_answers);
    println!("Num nodes: {}", num_nodes);
    let mut ofile = File::create("set_partition.dot")?;
    ofile.write_all(
        &dump_dot(
            &arena,
            &f.serialize(),
            "SetPartition",
            &DumpDotOption::default(),
        )
        .into_bytes(),
    )?;

    Ok(())
}
