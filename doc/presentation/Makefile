SHELL:=/usr/bin/env bash

watch: dependencies
	typst watch main.typ --root ../..

dependencies: $(shell echo ../figures/{architecture,egraph_example,egraph_cluster,passes_vs_peepholes,compilation_passes,peephole_example}.svg)

../figures/%.svg: ../figures/%.dot
	dot -Tsvg $^ > $@
