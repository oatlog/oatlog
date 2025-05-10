from typing import Dict, Tuple
from simple_union_find import UnionFind
from dataclasses import dataclass

@dataclass(unsafe_hash=True)
class EClass:
    val: int

class EGraph:
    union_find: UnionFind # U: the union-find of e-classes
    hashcons: Dict[Tuple, EClass] # H: e-node to e-class

    def __init__(self):
        self.union_find = UnionFind(0)
        self.hashcons = dict() 

    # unify e-classes a and b
    def union(self, a: EClass, b: EClass):
        self.union_find.union(a.val, b.val)

    # find representative of e-class
    def find(self, x: EClass) -> EClass:
        return EClass(self.union_find.find(x.val))

    # create new e-class
    def make(self) -> EClass:
        return EClass(self.union_find.make())

    # canonicalize e-graph
    def canonicalize(self):
        while True:
            changed = False
            new_hashcons = dict()
            for enode, eclass in self.hashcons.items():
                eclass = self.find(eclass)

                # canonicalize e-node
                enode = list(enode)
                for i in range(1, len(enode)):
                    enode[i] = self.find(enode[i])
                enode = tuple(enode)

                existing_eclass = new_hashcons.get(enode)
                if existing_eclass is not None:
                    self.union(eclass, existing_eclass)
                    changed = True
                else:
                    new_hashcons[enode] = eclass
            self.hashcons = new_hashcons
            if not changed:
                break

    # insert a new e-node, returns its e-class
    def insert_enode(self, enode: Tuple) -> EClass:
        existing_eclass = self.hashcons.get(enode)
        if existing_eclass is not None:
            return existing_eclass

        new_eclass = self.make()
        self.hashcons[enode] = new_eclass
        return new_eclass


e_graph = EGraph()

# e = (a + b) 
# f = (c + d)
a = e_graph.make()
b = e_graph.make()
c = e_graph.make()
d = e_graph.make()

e = e_graph.insert_enode(("+", a, b))
f = e_graph.insert_enode(("+", c, d))

e_graph.canonicalize()
assert e_graph.find(e) != e_graph.find(f)

# if we add that a = c and b = d then
# the e-graph should also unify e and f
e_graph.union(a, c)
e_graph.union(b, d)
e_graph.canonicalize()
assert e_graph.find(e) == e_graph.find(f)

