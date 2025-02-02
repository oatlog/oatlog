all: architecture.svg egraph_example.svg

%.svg: %.dot
	dot -Tsvg $^ > $@
