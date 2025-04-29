from typing import Dict, Tuple
from simple_union_find import UnionFind
from dataclasses import dataclass
from enum import Enum

@dataclass(unsafe_hash=True)
class EClass:
    val: int

class EGraph:

    union_find: UnionFind # U: the union-find of e-classes
    hashcons: Dict[Tuple, EClass] # H: e-node to e-class

    def __init__(self):
        self.union_find = UnionFind(0)
        self.hashcons = dict() 

    def make_eclass(self):
        return EClass(self.union_find.make())

    # claim that e-classes a and b are the same
    def unify(self, a: EClass, b: EClass):
        self.union_find.union(a.val, b.val)

    # find representative of e-class
    def find(self, x: EClass) -> EClass:
        return EClass(self.union_find.find(x.val))

    # insert a new e-node, returns its e-class
    def insert_enode(self, enode: Tuple) -> EClass:
        existing_eclass = self.hashcons.get(enode)
        if existing_eclass is not None:
            return existing_eclass

        new_eclass = self.make_eclass()
        self.hashcons[enode] = new_eclass
        return new_eclass

    # canonicalize e-graph 
    def canonicalize(self):
        def canonicalize_element(x):
            if type(x) is EClass:
                return self.find(x)
            else:
                return x

        while True:
            done = True
            new_hashcons = dict()
            for enode, eclass in self.hashcons.items():
                eclass = self.find(eclass)
                new_enode = tuple(map(
                    canonicalize_element, enode))

                existing_eclass = new_hashcons.get(new_enode)
                if existing_eclass is not None:
                    self.unify(eclass, existing_eclass)
                    done = False
                else:
                    new_hashcons[new_enode] = eclass
            self.hashcons = new_hashcons
            if done:
                break

class Label(Enum):
    ADD = 1
    SUB = 2
    MUL = 3
    DIV = 4
    SQRT = 5

e_graph = EGraph()

# e = (a + b) 
# f = (c + d)
a = e_graph.make_eclass()
b = e_graph.make_eclass()
c = e_graph.make_eclass()
d = e_graph.make_eclass()

e = e_graph.insert_enode((Label.ADD, a, b))
f = e_graph.insert_enode((Label.ADD, c, d))

e_graph.canonicalize()
assert e != f

# if we add that a = c and b = d then
# the e-graph should also unify e and f
e_graph.unify(a, c)
e_graph.unify(b, d)
e_graph.canonicalize()
assert e_graph.find(e) == e_graph.find(f)

