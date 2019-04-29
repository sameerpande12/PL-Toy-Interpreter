all:
	ocamllex a2.mll
	ocamlc -c a2.ml
	ocamlc -c a6.ml

clean:
	rm  *.cmo *.cmi 
