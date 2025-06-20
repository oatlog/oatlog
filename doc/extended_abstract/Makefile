SHELL:=/usr/bin/env bash

watch: dependencies
	typst watch main.typ --root ../..

dependencies: $(shell echo ../figures/{architecture,egraph_example,egraph_cluster}.svg)

../figures/%.svg: ../figures/%.dot
	dot -Tsvg $^ > $@

RG := rg --glob '*.typ' --multiline

lint:
	@if $(RG) '^=+ [^\n]+\n+='; \
		then echo "ERROR: Empty section forbidden!"; exit 1; fi
	@if $(RG) '^=+ [^\n]+\n[^\n]'; \
		then echo "ERROR: Newlines are required after headings!"; exit 1; fi
	@if $(RG) '\n\n\n'; \
		then echo "ERROR: No two blank lines in a row please!"; exit 1; fi
	@if $(RG) 's-expression'; \
		then echo "ERROR: The S in S-expression should be capitalized!"; exit 1; fi
	@if $(RG) '[^`"_]datalog'; \
		then echo "ERROR: The D in Datalog should be capitalized!"; exit 1; fi
	@if $(RG) '[^`"]rust[^c]'; \
		then echo "ERROR: The R in Rust should be capitalized!"; exit 1; fi
	@if $(RG) '[^= \.] Oatlog'; \
		then echo "ERROR: Only ever capitalize oatlog at the start of a sentence!"; exit 1; fi
	@if $(RG) '[^= \.:] E-'; \
		then echo "ERROR: Only ever capitalize E-graph/node/class at the start of a sentence!"; exit 1; fi
	@if $(RG) '^#table'; \
		then echo "ERROR: All tables should sit within figures!"; exit 1; fi
	@if $(RG) '^#figure\(\n([^\)].*\n)+\)\n'; \
		then echo "ERROR: All figures should be labelled!"; exit 1; fi
