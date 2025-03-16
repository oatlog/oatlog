watch:
	typst watch main.typ --root ../..

RG := rg --glob '!mastery-chs' --glob '!Makefile' --multiline

lint:
	@if $(RG) '^=+ [^\n]+\n+='; \
		then echo "ERROR: Empty section forbidden!"; exit 1; fi
	@if $(RG) '^=+ [^\n]+\n[^\n]'; \
		then echo "ERROR: Newlines are required after headings!"; exit 1; fi
	@if $(RG) '\n\n\n'; \
		then echo "ERROR: No two blank lines in a row please!"; exit 1; fi
	@if $(RG) 's-expression'; \
		then echo "ERROR: The S in S-expression should be capitalized!"; exit 1; fi
	@if $(RG) --glob '*.typ' '[^`"]rust[^c]'; \
		then echo "ERROR: The R in Rust should be capitalized!"; exit 1; fi
