OCAMLC=ocamlfind ocamlc
OCAMLOPT=ocamlfind ocamlopt
PACKAGES=str,unix,lp,lp-glpk

all: my_program

main.cmo: main.ml 
	$(OCAMLC) -c -package $(PACKAGES) $<

main.cmx: main.ml 
	$(OCAMLOPT) -c -package $(PACKAGES) $<

my_program: main.cmo
	$(OCAMLC) -o $@ -package $(PACKAGES) -linkpkg $^

my_program.opt: main.cmx
	$(OCAMLOPT) -o $@ -package $(PACKAGES) -linkpkg $^

clean:
	rm -f *.cm[iox] *.o my_program my_program.opt
