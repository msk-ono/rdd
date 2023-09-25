# rdd
Decision diagram library for Rust.

Currently supported DDs are as follows.

* Binary Decision Diagram (BDD)

## Test

```sh
$ cat examples/boolean_formula/median.txt | cargo run
Num answers: 4
Num nodes: 6
Graphviz: 
digraph BDD {
    n10 [label="A"];
    n10 -> n6 [style=dotted];
    n10 -> n9;
    n9 [label="B"];
    n9 -> n4 [style=dotted];
    n9 -> n1;
    n4 [label="C"];
    n4 -> n1;
    n6 [label="B"];
    n6 -> n4;
    n1 [label="⊤", shape = box];
}
```

## Example

See [examples/README.md](examples/README.md).
