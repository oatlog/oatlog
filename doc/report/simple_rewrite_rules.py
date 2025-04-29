from typing import List
from simple_egraph import EClass, EGraph, Label

# (a + b) * c = a * c + b * c
def distributive_law(e_graph: EGraph, to_insert: List):
    # t1 = (a + b) * c
    for enode, t1 in e_graph.hashcons.items():
        if enode[0] != Label.MUL:
            continue

        # t0 = a + b
        _, t0, c = enode
        for enode, eclass in e_graph.hashcons.items():
            if enode[0] != Label.ADD or eclass != t0:
                continue

        # We have now matched (a + b) * c
        # so lets schedule an insert of a * c + b * c

        t2 = e_graph.make_eclass()
        t3 = e_graph.make_eclass()
        to_insert.append(((Label.MUL, a, c), t2)) # t2 = a * c
        to_insert.append(((Label.MUL, b, c), t3)) # t3 = b * c
        to_insert.append(((Label.ADD, t2, t3), t1)) # t1 = (a * c) * (b * c)
        #                                      ^^
        # NOTE: using t1 here makes canonicalize unify the lhs and rhs of
        # the rewrite rule

# a + b = a * c + b * c
def commutative_laws(e_graph: EGraph, to_insert: List):
    for enode, eclass in e_graph.hashcons.items():
        if enode[0] == Label.MUL:
            _, a, b = enode
            to_insert.append(((Label.MUL, b, a), eclass))
        if enode[0] == Label.ADD:
            _, a, b = enode
            to_insert.append(((Label.ADD, b, a), eclass))

def apply_rules(e_graph: EGraph):
    to_insert = []

    distributive_law(e_graph, to_insert)
    commutative_laws(e_graph, to_insert)

    for (enode, eclass) in to_insert:
        e_graph.unify(e_graph.insert_enode(enode), eclass)

e_graph = EGraph()

a = e_graph.make_eclass()
b = e_graph.make_eclass()
c = e_graph.make_eclass()
d = e_graph.make_eclass()

def mul(a: EClass, b: EClass) -> EClass:
    return e_graph.insert_enode((Label.MUL, a, b))

def add(a: EClass, b: EClass) -> EClass:
    return e_graph.insert_enode((Label.MUL, a, b))

# (a + b) * (c + d)
factored = mul(add(a, b), add(c, d))

# c * (b + a) + (a + b) * d
expanded_commuted = add(mul(add(a, b), c), mul(add(a, b), d))

e_graph.canonicalize()

assert e_graph.find(factored) != e_graph.find(expanded_commuted)

for _ in range(5):
    apply_rules(e_graph)
    e_graph.canonicalize()

assert e_graph.find(factored) == e_graph.find(expanded_commuted)
