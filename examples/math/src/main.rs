


fn main() {

        egraph::compile_egraph!((
            (datatype Math
             (Mul Math Math)
             (Add Math Math)
            )
        ));
    /*
    // TODO: egglog has builtin include, so just use that.
    egraph::compile_egraph!((
            (datatype Math
                (Mul Math Math)
                (Add Math Math)
                (Const i64)
            )
            // (let one (Const 1))
            (rewrite (Add a b) (Add b a))
            */

    /*

    https://www.youtube.com/watch?v=9epgZ-e6DUU
    https://inria.hal.science/hal-01723236v1/document

    (sort Data)
    (sort Phi)
    (sort Region)
    (sort Control)
    (sort Jump)
    (sort Cond)
    (sort Branch)

    (sort Start)

    (function Start () Start)


    (sort DataVec (Vec Data))
    (sort ControlVec (Vec Control))


    (function Const (i64) Data)
    (function Add (Data Data) Data)
    (function Sub (Data Data) Data)
    (function Mul (Data Data) Data)
    (function Div (Data Data) Data)
    (function Eq (Data Data) Data)


    (function Phi (Region, DataVec) Phi)

    (function Region (ControlVec) Region)

    (function Return (Region Data)
    (function Jump (Region) Control)
    (function If (Region Data) Cond)
    (function IfTrue (Cond) Branch)
    (function IfFalse (Cond) Branch)





    // https://github.com/SeaOfNodes/Simple/blob/main/chapter03/README.md
    // (sort Control)
    // (sort Data)


    // (function Start (ControlToken Data) Control)
    // (function Return (Control, Data) Control)
    // (function Constant (i64) Data)
    // (function Add (Data Data) Data)
    // (function Sub (Data Data) Data)
    // (function Mul (Data Data) Data)
    // (function Div (Data Data) Data)
    // (function Negate (Data) Data)



    (function Start () ())

    // predecessor, data to return
    (function Return (Control Data) Control)
    (



    https://rust-lang.github.io/polonius/rules/atoms.html

    (sort Path)

    (function PathVar (Variable) Path)
    (function PathField (Path Field) Path) //
    (function PathIndex (Path) Path) // x[something], but borrow checker does not track what that
                                     // something is
    (function PathDeref (Path) Path)


    // https://www.irisa.fr/celtique/ext/sea-of-nodes/

    (sort Memory)
    (sort Nat)

    (function MemRead (Memory Nat) Nat)

    (function MemWrite (Memory Nat Nat) Memory)


    (sort Val)

    // TODO: would be nice if Val = Memory | Nat
    //       essentially duplicating rules?
    (function ValMem (Memory) Val)
    (function ValNat (Nat) Val)

    (sort Node)

    (sort Id)

    (sort IdVec (Vec Id))

    (function Region (IdVec) Node)
    (function Cst (Val) Node)
    (function BinOp (Id Id) Node)
    (function Phi (Id IdVec) Node)
    (function Store (Id Id Id) Node)
    (function Load (Id Id Id Node)
    (function Jmp (Id) Node)
    (function If (Id) Node)
    (function IfT (Id) Node)
    (function IfF (Id) Node)
    (function Return (Id Id) Node)
    (function ZeroCheck (Id Id) Node)


    */
    /*
    ));
    */
}
