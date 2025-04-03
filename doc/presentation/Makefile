SHELL:=/usr/bin/env bash

watch: dependencies
	typst watch main.typ --root ../..

D=architecture,egraph_example,egraph_cluster,passes_vs_peepholes,compilation_passes,peephole_example,egraph_example0,egraph_example1,egraph_example2,egraph_example3
dependencies: $(shell echo ../figures/{$D}.svg)

../figures/%.svg: ../figures/%.dot
	dot -Tsvg $^ > $@
