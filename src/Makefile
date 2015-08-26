
all:build 

test: 
	ocamlbuild -use-ocamlfind SymCaml.native
	mv SymCaml.native symcaml_test
	chmod +x symcaml_test


install:
	ocaml setup.ml -install 

build:setup.data setup.ml
	ocaml setup.ml -build

setup.data:setup.ml
	ocaml setup.ml -configure

setup.ml:_oasis
	oasis setup -setup-update dynamic

clean:
	rm -f setup.* _oasis
