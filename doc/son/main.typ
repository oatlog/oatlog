
= Sea of nodes
https://inria.hal.science/hal-01723236/file/sea-of-nodes-hal.pdf

```
node = data | region | control | branch

data = Cst_n | binop | phi

binop = Add (x, y) | Eq (x, y)

phi = Phi(r, x_1, ..., x_m)

region = Region(p_1, ..., p_m)

control = jump | cond | Return(r, x)

jump = Jmp(r)

cond = If (r, x)

branch = IfT(if) | IfF(if)

notation:
data: x, y, z, w
phi: phi
r: region
c: control
if: cond
p: jump or branch    
n: node
```

```rust
enum Node {
    Data(&Data),
    Region(&Region),
    Control(&Control),
    Branch(&Branch),
}

enum Data {
    Constant(&i64),
    BinOp(&BinOp),
    Phi(&Phi),
}

enum BinOp {
    Add(&Data, &Data),
    Eq(&Data, &Data),
}

// vec arity matches region.
struct Phi(&Region, Vec<&Data>);

// "come from" edges.
struct Region(Vec<&JumpOrBranch>);

// exactly one region may depend on this.
enum JumpOrBranch {
    Jump(&Jump),
    Branch(&Branch),
}

enum Control {
    Jump(&Jump),
    Cond(&Cond),
    Return(&Region, &Data),
}

// jump from this region to region that points to us.
// (through JumpOrBranch).
struct Jump(&Region);

struct Cond(&Region, &Data);

enum Branch {
    IfT(&Cond),
    IfF(&Cond),
}
```

extend with coq proofs

```rust
enum Val {

}


```
