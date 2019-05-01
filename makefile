all:
	ocamllex lexer.mll
	ocamlopt -o exe lexer.ml a6.ml interpreter.ml
clean:
	rm  lexer.ml *.cmi *.cmx *.o exe
