all: architecture.svg egraph_example.svg

%.svg: %.dot
	dot -Tsvg $^ > $@

check:
	grep -v -f banned_words.txt *.typ
