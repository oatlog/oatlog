= Semantic foundations of equality saturation
https://arxiv.org/abs/2501.02413

"Equality saturation is an emerging technique for program and *query optimization* developed in the programming language community."
Should we be using it for query optimization?


Fixpoint semantics of equality saturation based on tree automata.


Given equalites between terms $t_1, t_2$, check if that implies $t_1 eqiv t_2$ .

Nelson introduced e-graphs
https://scottmcpeak.com/nelson-verification.pdf

egglog semantics are based on fixpoints? ("unifying datalog and equality saturation")

Current semantics are based on an imperative algorithm, which is hard to analyze.

Termination problem: given rewrite rules, does it terminate on all inputs e-graphs?

Proof that if fixpoint is finite, e-graph terminates in a finite number of steps.


E-class is the state of a tree automata.

E-node is a transition of the tree automata (so adding it changed the e-class state).

these tree automata do not have any final state.

"It is folklore that E-graphs represent equivalences of terms."

The chase procedure.




= Equality Saturation Theory Exploration à la Carte
User guided automated rewrite rule generation



