# rdd
Decision diagram library for Rust.

Currently supported DDs are as follows.

* Binary Decision Diagram (BDD)

## Test

```sh
$ cat examples/boolean_formula/median.txt | cargo run
Num answers: 4
Num nodes: 6
Sample answers:
B: true, A: true, 
B: true, A: false, C: true, 
B: true, A: true, 
B: true, A: true, 
B: false, A: true, C: true, 
Graphviz:
digraph BDD {
    n5 [label="A"];
    n4 [label="B"];
    n3 [label="B"];
    n2 [label="C"];
    n1 [label="⊤", shape = box];
    n5 -> n3 [style=dashed];
    n5 -> n4 [style=solid];
    n4 -> n2 [style=dashed];
    n4 -> n1 [style=solid];
    n3 -> n2 [style=solid];
    n2 -> n1 [style=solid];
}
```

## Example

See [examples/README.md](examples/README.md).
