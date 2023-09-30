use fnv::{FnvHashMap, FnvHashSet};
use rand::rngs::ThreadRng;
use rand::{distributions::Uniform, Rng};
use std::{collections::LinkedList, ops, vec};

/// Id type of variable.
pub type VarID = u32;
/// Id type of node.
type NodeID = u32;
/// Id of false node.
const FALSE_NODE_ID: NodeID = 0;
/// Id of true node.
const TRUE_NODE_ID: NodeID = 1;
/// Boolean variable.
#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
pub struct BoolVar(VarID);
impl BoolVar {
    pub const fn new(x: VarID) -> Self {
        BoolVar(x)
    }
    pub const fn x(&self) -> VarID {
        self.0
    }
}
/// BDD.
///
/// BDD is wrapper of BDDNode.
#[derive(Debug, Clone, Copy)]
pub struct BDD {
    node: *const BDDNode,
    arena: *mut BDDArena,
}
impl BDD {
    fn new(node: *const BDDNode, arena: &mut BDDArena) -> Self {
        BDD { node, arena }
    }
    fn arena(&self) -> &BDDArena {
        unsafe { self.arena.as_ref().unwrap() }
    }
    fn arena_mut(&self) -> &mut BDDArena {
        unsafe { self.arena.as_mut().unwrap() }
    }
    fn node(&self) -> &BDDNode {
        unsafe { self.node.as_ref().unwrap() }
    }
    pub fn lo(&self) -> Option<BDD> {
        if self.node().is_constant() {
            None
        } else {
            Some(BDD {
                node: self.node().lo().unwrap(),
                arena: self.arena,
            })
        }
    }
    pub fn hi(&self) -> Option<BDD> {
        if self.node().is_constant() {
            None
        } else {
            Some(BDD {
                node: self.node().hi().unwrap(),
                arena: self.arena,
            })
        }
    }
    pub fn var(&self) -> BoolVar {
        self.node().var()
    }
    pub fn num_answers(&self) -> u64 {
        let num_vars = self.arena().vars.len() as u64;
        let node = self.node();
        if node.is_false() {
            0u64
        } else if node.is_true() {
            if num_vars == 0u64 {
                0u64
            } else {
                1u64 << num_vars
            }
        } else {
            (1u64 << node.var().x()) * node.num_answers(num_vars)
        }
    }
    pub fn num_nodes(&self) -> u32 {
        self.node().num_nodes()
    }
    pub fn serialize(&self) -> SerializedBDD {
        let mut map = FnvHashMap::default();
        self.node().convert_to_map(&mut map);

        let num_vars = self.arena().num_vars();
        let var = BoolVar::new(num_vars as VarID);
        let mut sbdd = SerializedBDD { nodes: vec![] };
        let mut id2idx = FnvHashMap::default();
        sbdd.nodes.push(BranchNode::f(var));
        id2idx.insert(FALSE_NODE_ID, 0);
        sbdd.nodes.push(BranchNode::t(var));
        id2idx.insert(TRUE_NODE_ID, 1);
        // FIXME(msk-ono): Stop iterating map for many times.
        for var in (0..num_vars).rev() {
            let var = BoolVar::new(var as VarID);
            for (id, (tmp, lo, hi)) in map.iter() {
                if *tmp != var || id2idx.contains_key(id) {
                    continue;
                }
                id2idx.insert(*id, sbdd.nodes.len());
                sbdd.nodes.push(BranchNode::new(
                    var,
                    *id2idx.get(lo).unwrap(),
                    *id2idx.get(hi).unwrap(),
                ));
            }
        }

        sbdd
    }
}
impl ops::BitXor for BDD {
    type Output = Self;
    fn bitxor(mut self, rhs: Self) -> Self::Output {
        self ^= rhs;
        self
    }
}
impl ops::BitXorAssign for BDD {
    fn bitxor_assign(&mut self, rhs: Self) {
        let lhs = self.node;
        let node = self.arena_mut().apply(&|a, b| a ^ b, lhs, rhs.node);
        *self = BDD::new(node, self.arena_mut());
    }
}
impl ops::BitAnd for BDD {
    type Output = Self;
    fn bitand(mut self, rhs: Self) -> Self::Output {
        self &= rhs;
        self
    }
}
impl ops::BitAndAssign for BDD {
    fn bitand_assign(&mut self, rhs: BDD) {
        let lhs = self.node;
        let node = self.arena_mut().apply(&|a, b| a & b, lhs, rhs.node);
        *self = BDD::new(node, self.arena_mut());
    }
}
impl ops::BitOr for BDD {
    type Output = Self;
    fn bitor(mut self, rhs: Self) -> Self::Output {
        self |= rhs;
        self
    }
}
impl ops::BitOrAssign for BDD {
    fn bitor_assign(&mut self, rhs: BDD) {
        let lhs = self.node;
        let node = self.arena_mut().apply(&|a, b| a | b, lhs, rhs.node);
        *self = BDD::new(node, self.arena_mut());
    }
}
impl ops::Not for BDD {
    type Output = Self;
    fn not(self) -> Self::Output {
        self.arena_mut().true_bdd() ^ self
    }
}
#[derive(Debug)]
struct BDDNode {
    id: NodeID,
    var: BoolVar,
    lo: *const BDDNode,
    hi: *const BDDNode,
}
impl BDDNode {
    fn id(&self) -> NodeID {
        self.id
    }
    fn var(&self) -> BoolVar {
        self.var
    }
    fn is_false(&self) -> bool {
        self.id == FALSE_NODE_ID
    }
    fn is_true(&self) -> bool {
        self.id == TRUE_NODE_ID
    }
    fn is_constant(&self) -> bool {
        self.is_false() || self.is_true()
    }
    fn lo(&self) -> Option<&BDDNode> {
        if self.is_constant() {
            None
        } else {
            unsafe { Some(self.lo.as_ref().unwrap()) }
        }
    }
    fn hi(&self) -> Option<&BDDNode> {
        if self.is_constant() {
            None
        } else {
            unsafe { Some(self.hi.as_ref().unwrap()) }
        }
    }
    fn value(&self) -> Option<bool> {
        if self.is_false() {
            Some(false)
        } else if self.is_true() {
            Some(true)
        } else {
            None
        }
    }
    fn num_answers(&self, num_vars: u64) -> u64 {
        if self.is_true() {
            1u64
        } else if self.is_false() {
            0u64
        } else {
            let var = self.var().x();
            let lo = self.lo().unwrap();
            let num_lo = lo.num_answers(num_vars);
            let scale_lo = 1u64 << (lo.var().x() - var - 1);
            let hi = self.hi().unwrap();
            let num_hi = hi.num_answers(num_vars);
            let scale_hi = 1u64 << (hi.var().x() - var - 1);
            scale_lo * num_lo + scale_hi * num_hi
        }
    }
    fn convert_to_map(&self, map: &mut FnvHashMap<NodeID, (BoolVar, NodeID, NodeID)>) {
        if self.is_constant() {
            if self.is_false() {
                map.insert(FALSE_NODE_ID, (self.var(), FALSE_NODE_ID, FALSE_NODE_ID));
            } else if self.is_true() {
                map.insert(TRUE_NODE_ID, (self.var(), TRUE_NODE_ID, TRUE_NODE_ID));
            }
            return;
        }

        let lo = self.lo().unwrap();
        let hi = self.hi().unwrap();
        map.insert(self.id(), (self.var(), lo.id(), hi.id()));
        lo.convert_to_map(map);
        hi.convert_to_map(map);
    }
    fn insert_all_nodes(&self, set: &mut FnvHashSet<NodeID>) {
        let mut stack = vec![self];
        while stack.len() > 0 {
            let node = stack.pop().unwrap();
            set.insert(node.id());
            if node.lo().is_some() {
                stack.push(node.lo().unwrap())
            }
            if node.hi().is_some() {
                stack.push(node.hi().unwrap())
            }
        }
    }
    fn num_nodes(&self) -> u32 {
        let mut set = FnvHashSet::default();
        self.insert_all_nodes(&mut set);
        set.len() as u32
    }
}
#[derive(Debug, PartialEq, Eq, Hash)]
struct ReverseMapKey {
    var: BoolVar,
    lo: NodeID,
    hi: NodeID,
}
#[derive(Debug)]
pub struct BDDArena {
    node_list: LinkedList<BDDNode>,
    id2node: FnvHashMap<NodeID, *const BDDNode>,
    reverse_map: FnvHashMap<ReverseMapKey, *const BDDNode>,
    vars: Vec<String>,
}
type Operation = dyn Fn(bool, bool) -> bool;
impl BDDArena {
    pub fn new() -> Self {
        let mut arena = BDDArena {
            node_list: LinkedList::new(),
            reverse_map: FnvHashMap::default(),
            id2node: FnvHashMap::default(),
            vars: vec![],
        };
        let false_node = BDDNode {
            id: FALSE_NODE_ID,
            var: BoolVar::new(0),
            lo: std::ptr::null(),
            hi: std::ptr::null(),
        };
        let true_node = BDDNode {
            id: TRUE_NODE_ID,
            var: BoolVar::new(0),
            lo: std::ptr::null(),
            hi: std::ptr::null(),
        };
        arena.node_list.push_back(false_node);
        arena
            .id2node
            .insert(FALSE_NODE_ID, arena.node_list.back().unwrap());
        arena.node_list.push_back(true_node);
        arena
            .id2node
            .insert(TRUE_NODE_ID, arena.node_list.back().unwrap());

        arena
    }
    pub fn new_var(&mut self, name: String) -> BDD {
        let var = BoolVar::new(self.vars.len() as VarID);
        self.vars.push(name);
        let mut iter = self.node_list.iter_mut();
        iter.next().unwrap().var = BoolVar::new(var.x() + 1); // Set var of false_node.
        iter.next().unwrap().var = BoolVar::new(var.x() + 1); // Set var of true_nod.

        BDD::new(self.mk(var, self.false_node(), self.true_node()), self)
    }
    pub fn name(&self, var: BoolVar) -> &str {
        &self.vars[var.x() as usize]
    }
    pub fn num_vars(&self) -> usize {
        self.vars.len()
    }
    pub fn false_bdd(&mut self) -> BDD {
        BDD::new(self.false_node(), self)
    }
    pub fn true_bdd(&mut self) -> BDD {
        BDD::new(self.true_node(), self)
    }
    fn false_node(&self) -> *const BDDNode {
        *self.id2node.get(&FALSE_NODE_ID).unwrap()
    }
    fn true_node(&self) -> *const BDDNode {
        *self.id2node.get(&TRUE_NODE_ID).unwrap()
    }
    fn mk(&mut self, var: BoolVar, lo: *const BDDNode, hi: *const BDDNode) -> *const BDDNode {
        let lo = unsafe { lo.as_ref().unwrap() };
        let hi = unsafe { hi.as_ref().unwrap() };
        if lo.id() == hi.id() {
            lo
        } else {
            let key = ReverseMapKey {
                var: var.clone(),
                lo: lo.id(),
                hi: hi.id(),
            };
            match self.reverse_map.get(&key) {
                Some(bdd) => *bdd,
                None => {
                    let node = BDDNode {
                        id: self.node_list.len() as NodeID,
                        var,
                        lo,
                        hi,
                    };
                    self.node_list.push_back(node);
                    let node = self.node_list.back().unwrap();
                    self.reverse_map.insert(key, node);
                    self.id2node.insert(node.id, node);

                    node
                }
            }
        }
    }
    fn apply(&mut self, fkt: &Operation, x: *const BDDNode, y: *const BDDNode) -> *const BDDNode {
        let x = unsafe { x.as_ref().unwrap() };
        let y = unsafe { y.as_ref().unwrap() };
        if x.is_constant() && y.is_constant() {
            if fkt(x.value().unwrap(), y.value().unwrap()) {
                self.true_node()
            } else {
                self.false_node()
            }
        } else if x.var() == y.var() {
            let lo = self.apply(fkt, x.lo().unwrap(), y.lo().unwrap());
            let hi = self.apply(fkt, x.hi().unwrap(), y.hi().unwrap());
            self.mk(x.var(), lo, hi)
        } else if x.var() < y.var() {
            let lo = self.apply(fkt, x.lo().unwrap(), y);
            let hi = self.apply(fkt, x.hi().unwrap(), y);
            self.mk(x.var(), lo, hi)
        } else {
            // x.var() > y.var()
            let lo = self.apply(fkt, x, y.lo().unwrap());
            let hi = self.apply(fkt, x, y.hi().unwrap());
            self.mk(y.var(), lo, hi)
        }
    }
}
#[derive(Debug)]
pub struct BranchNode {
    pub var: BoolVar,
    pub lo: usize,
    pub hi: usize,
}
impl BranchNode {
    fn new(var: BoolVar, lo: usize, hi: usize) -> Self {
        BranchNode { var, lo, hi }
    }
    fn f(var: BoolVar) -> Self {
        BranchNode::new(var, 0, 0)
    }
    fn t(var: BoolVar) -> Self {
        BranchNode::new(var, 1, 1)
    }
}
#[derive(Debug)]
pub struct SerializedBDD {
    nodes: Vec<BranchNode>,
}
impl SerializedBDD {
    pub fn num_nodes(&self) -> usize {
        self.nodes.len()
    }
    pub fn get(&self, idx: usize) -> Option<&BranchNode> {
        self.nodes.get(idx)
    }
    pub fn calc_edge_weight(&self) -> Vec<(u64, u64)> {
        let mut w = vec![(0, 0); self.num_nodes()];
        w[1] = (0, 1);
        for i in 2..self.num_nodes() {
            let n = &self.nodes[i];
            let lo = w[n.lo].0 + w[n.lo].1;
            let hi = w[n.hi].0 + w[n.hi].1;
            let scale_lo = 1u64 << (self.nodes[n.lo].var.x() - n.var.x() - 1);
            let scale_hi = 1u64 << (self.nodes[n.hi].var.x() - n.var.x() - 1);
            w[i] = (lo * scale_lo, hi * scale_hi);
        }
        w
    }
    pub fn num_answers(&self) -> u64 {
        let w = self.calc_edge_weight();
        let entry = w.last().unwrap();
        entry.0 + entry.1
    }
}
pub fn sample_answer_from_bdd(
    sbdd: &SerializedBDD,
    edge_weight: &Vec<(u64, u64)>,
    rng: &mut ThreadRng,
) -> FnvHashMap<BoolVar, bool> {
    let mut map = FnvHashMap::default();
    let distr = Uniform::new(0.0, 1.0);
    let mut idx = sbdd.nodes.len() - 1;
    while idx != 1 {
        let n = &sbdd.nodes[idx];
        let (w_lo, w_hi) = edge_weight[idx];
        let prob = w_lo as f64 / ((w_lo + w_hi) as f64);
        if rng.sample(distr) <= prob {
            map.insert(n.var, false);
            idx = n.lo;
        } else {
            map.insert(n.var, true);
            idx = n.hi;
        }
    }
    map
}
pub fn sample_answers_from_bdd(
    sbdd: &SerializedBDD,
    rng: &mut ThreadRng,
    num_samples: usize,
) -> Vec<FnvHashMap<BoolVar, bool>> {
    let edge_weight = sbdd.calc_edge_weight();
    let mut samples = vec![];
    for _ in 0..num_samples {
        samples.push(sample_answer_from_bdd(sbdd, &edge_weight, rng));
    }
    samples
}
pub struct DumpDotOption {
    pub dump_weight: bool,
    pub edge_style_lo: String,
    pub edge_style_hi: String,
}
impl DumpDotOption {
    pub fn new(dump_weight: bool, edge_style_lo: &str, edge_style_hi: &str) -> Self {
        DumpDotOption {
            dump_weight,
            edge_style_lo: edge_style_lo.to_string(),
            edge_style_hi: edge_style_hi.to_string(),
        }
    }
    pub fn default() -> Self {
        DumpDotOption::new(false, "dashed", "solid")
    }
}
pub fn dump_dot(
    arena: &BDDArena,
    sbdd: &SerializedBDD,
    title: &str,
    option: &DumpDotOption,
) -> String {
    let mut ret = String::new();
    ret.reserve(1024 * 1024); // Allocate 1MiB.
    ret += &format!("digraph {} {{\n", title); // Prefix.

    // Dump nodes.
    for (idx, n) in sbdd.nodes.iter().enumerate().rev() {
        if idx == 0 {
            // ret += &format!("    n{} [label=\"⊥\", shape = box];\n", FALSE_NODE_ID);
        } else if idx == 1 {
            ret += &format!("    n{} [label=\"⊤\", shape = box];\n", TRUE_NODE_ID);
        } else {
            ret += &format!("    n{} [label=\"{}\"];\n", idx, arena.name(n.var));
        }
    }
    // Dump edges.
    for (idx, n) in sbdd.nodes.iter().enumerate().rev() {
        // Constant nodes are terminal.
        if idx == 0 || idx == 1 {
            continue;
        }

        // TODO(msk-ono): Implement dump_weight option.
        if n.lo != 0 {
            ret += &format!(
                "    n{} -> n{} [style={}];\n",
                idx, n.lo, option.edge_style_lo
            );
        }
        if n.hi != 0 {
            ret += &format!(
                "    n{} -> n{} [style={}];\n",
                idx, n.hi, option.edge_style_hi
            );
        }
    }
    ret += "}\n"; // Suffix.

    ret
}

#[cfg(test)]
mod test {
    use super::BDDArena;
    use super::{FALSE_NODE_ID, TRUE_NODE_ID};

    #[test]
    fn test_false_bdd() {
        let mut arena = BDDArena::new();
        let false_bdd = arena.false_bdd();
        assert_eq!(0, arena.num_vars());
        assert_eq!(FALSE_NODE_ID, false_bdd.node().id());
        assert!(false_bdd.node().is_false());
        assert!(!false_bdd.node().is_true());
        assert!(false_bdd.node().lo().is_none());
        assert!(false_bdd.node().hi().is_none());
        assert_eq!(0, false_bdd.num_answers());
        assert_eq!(1, false_bdd.num_nodes());
    }
    #[test]
    fn test_true_bdd() {
        let mut arena = BDDArena::new();
        let true_bdd = arena.true_bdd();
        assert_eq!(0, arena.num_vars());
        assert_eq!(TRUE_NODE_ID, true_bdd.node().id());
        assert!(!true_bdd.node().is_false());
        assert!(true_bdd.node().is_true());
        assert!(true_bdd.node().lo().is_none());
        assert!(true_bdd.node().hi().is_none());
        assert_eq!(0, true_bdd.num_answers());
        assert_eq!(1, true_bdd.num_nodes());
    }
    #[test]
    fn test_var() {
        let mut arena = BDDArena::new();
        let x = arena.new_var("x".to_string());
        assert_eq!(1, arena.num_vars());
        assert_eq!(1, x.num_answers());
        assert_eq!(3, x.num_nodes());
    }
    #[test]
    fn test_vars() {
        let mut arena = BDDArena::new();
        let x = arena.new_var("x".to_string());
        let y = arena.new_var("y".to_string());
        assert_eq!(2, arena.num_vars());
        assert_eq!(2, x.num_answers());
        assert_eq!(3, x.num_nodes());
        assert_eq!(2, y.num_answers());
        assert_eq!(3, y.num_nodes());
    }
    #[test]
    fn test_c6() {
        let mut arena = BDDArena::new();
        let num_vertices = 6 as usize;

        // Define variables.
        let mut v = vec![];
        for i in 0..num_vertices {
            v.push(arena.new_var(i.to_string()));
        }

        // Define independent set of C6.
        let mut exp = arena.false_bdd();
        for i in 0..num_vertices {
            exp |= v[i] & v[(i + 1) % num_vertices];
        }
        exp = !exp;
        assert_eq!(18, exp.num_answers());
        assert_eq!(16, exp.num_nodes());
        let sbdd = exp.serialize();
        assert_eq!(18, sbdd.num_answers());
        assert_eq!(16, sbdd.num_nodes());

        // Define kernel of C6.
        let mut exists = arena.true_bdd();
        for i in 0..num_vertices {
            exists &= v[(num_vertices + i - 1) % num_vertices] | v[i] | v[(i + 1) % num_vertices];
        }
        exp &= exists;
        assert_eq!(5, exp.num_answers());
        assert_eq!(17, exp.num_nodes());
        let sbdd = exp.serialize();
        assert_eq!(5, sbdd.num_answers());
        assert_eq!(17, sbdd.num_nodes());
    }
}
