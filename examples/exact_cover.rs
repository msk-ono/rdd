extern crate rdd;

use rdd::bdd::BDDArena;
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let height = 4;
    let width = 4;
    let mut arena = BDDArena::new();

    // Define variables.
    // NOTE: The order in which variables are defined is important.
    let mut horizontal_vars = vec![vec![arena.true_bdd(); width]; height];
    let mut vertical_vars = vec![vec![arena.true_bdd(); width]; height];
    for i in 0..(height + width) {
        for j in 0..=i {
            let x = j;
            let y = i - j;
            if x < height && y < width - 1 {
                horizontal_vars[x][y] = arena.new_var(format!("H[{}][{}]", x, y));
            }
            if x < height - 1 && y < width {
                vertical_vars[x][y] = arena.new_var(format!("V[{}][{}]", x, y))
            }
        }
    }

    // You cannot place vertically and horizontally at the same time.
    let mut constraint = arena.false_bdd();
    for i in 0..(height - 1) {
        for j in 0..(width - 1) {
            let h = horizontal_vars[i][j];
            let v = vertical_vars[i][j];
            constraint |= h & v;
        }
    }
    // If placed vertically, you cannot place in the square below.
    for i in 0..(height - 1) {
        for j in 0..width {
            let v = vertical_vars[i][j];
            if j != width - 1 {
                let low_h = horizontal_vars[i + 1][j];
                constraint |= v & low_h;
            }
            if i != height - 2 {
                let low_v = vertical_vars[i + 1][j];
                constraint |= v & low_v;
            }
        }
    }
    // If placed horizontally, you cannot place in the square to the right.
    for i in 0..height {
        for j in 0..(width - 1) {
            let h = horizontal_vars[i][j];
            if i != height - 1 {
                let right_v = vertical_vars[i][j + 1];
                constraint |= h & right_v;
            }
            if j != width - 2 {
                let right_h = horizontal_vars[i][j + 1];
                constraint |= h & right_h;
            }
        }
    }
    // Cover all squares.
    let mut cover = arena.true_bdd();
    for i in 0..height {
        for j in 0..width {
            let mut tmp_cover = arena.false_bdd();
            // Vertically place at (i - 1, j).
            if i != 0 {
                tmp_cover |= vertical_vars[i - 1][j];
            }
            // Horizontally place at (i, j - 1).
            if j != 0 {
                tmp_cover |= horizontal_vars[i][j - 1];
            }
            // Vertically place at (i, j).
            if i != height - 1 {
                tmp_cover |= vertical_vars[i][j];
            }
            // Horizontally place at (i, j).
            if j != width - 1 {
                tmp_cover |= horizontal_vars[i][j];
            }
            cover &= tmp_cover;
        }
    }

    let exp = (!constraint) & cover;
    let num_answers = exp.num_answers();
    assert_eq!(44, num_answers);
    let num_nodes = exp.num_nodes();
    assert_eq!(128, num_nodes);
    println!("Num answers: {}", num_answers);
    println!("Num nodes: {}", num_nodes);
    let mut ofile = File::create("exact_cover.dot")?;
    ofile.write_all(&exp.dump_graphviz("ExactCover", false).into_bytes())?;

    Ok(())
}
