
all:build 

install:
	ocaml setup.ml -install 

remove:
	ocaml setup.ml -uninstall
	ocamlfind remove pycaml

build:setup.data setup.ml
	ocaml setup.ml -build

setup.data:setup.ml
	ocaml setup.ml -configure

setup.ml:_oasis
	oasis setup -setup-update dynamic
_oasis:
	./init.sh

clean:
	rm -f setup.* _oasis
