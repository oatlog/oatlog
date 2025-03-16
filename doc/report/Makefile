watch:
	typst watch main.typ --root ../..

lint:
	@if rg --multiline '^=+ [^\n]+\n\w'; then echo "ERROR: Newlines are required after headings!"; fi
