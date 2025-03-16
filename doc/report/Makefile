watch:
	typst watch main.typ --root ../..

lint:
	@if rg --glob '!mastery-chs' --multiline '^=+ [^\n]+\n[^\n]'; \
		then echo "ERROR: Newlines are required after headings!"; exit 1; fi
