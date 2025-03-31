SHELL:=/usr/bin/env bash

watch: dependencies
	typst watch main.typ --root ../..

dependencies: $(shell echo ../figures/{architecture,egraph_example,egraph_cluster}.svg)

../figures/%.svg: ../figures/%.dot
	dot -Tsvg $^ > $@
