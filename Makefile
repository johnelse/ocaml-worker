all: build

ocamlfind_check=$(shell ocamlfind query $(1) > /dev/null 2> /dev/null && echo "true")

LWT=$(call ocamlfind_check,lwt)
ifeq ($(LWT), true)
	LWT_FLAG=--enable-lwt
endif

UNIX_FLAG=--enable-unix

TESTS_FLAG=--enable-tests

NAME=osc
J=4

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure $(LWT_FLAG) $(UNIX_FLAG) $(TESTS_FLAG)

build: setup.data setup.ml
	ocaml setup.ml -build -j $(J)

doc: setup.data setup.ml
	ocaml setup.ml -doc -j $(J)

install: setup.data setup.ml
	ocaml setup.ml -install

uninstall:
	ocamlfind remove $(NAME)

test: setup.ml build
	ocaml setup.ml -test

reinstall: setup.ml
	ocamlfind remove $(NAME) || true
	ocaml setup.ml -reinstall

clean:
	ocamlbuild -clean
	rm -f setup.data setup.log
