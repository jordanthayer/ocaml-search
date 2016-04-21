# The location of the ocaml source directory
OCAML_DIR=""

# The binaries
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKLIB=ocamlmklib
F2C=

INSTALL=install

# Enable debugging?
#DEBUG=-g
DEBUG=-DNDEBUG -O3

# The caml header files
INCLUDE=-I$(OCAML_DIR)

CFLAGS=$(DEBUG) $(INCLUDE) -fPIC -std=c99 -Wall
LDFLAGS=-lm

C_OBJ=				\
	libf2c.a		\
	bvls-ps.o

CAML_OBJ=			\
	bvls_stubs.o		\
	bvls.cmx		\
	bvls.cmo

INSTALL_OBJ=			\
	libf2c.a		\
	bvls.cmi		\
	bvls.cma		\
	bvls.cmxa		\
	libbvls.a		\
	bvls.a			\
	dllbvls.so


#
# How ocaml objects are built
#

%.cmi: %.mli
	$(OCAMLC) $^ -o $<

%.cmx: %.ml
	$(OCAMLOPT) -c $^ -o $<

%.cmo: %.ml
	$(OCAMLC) -c $^ -o $<

%.mli: %.ml
	$(OCAMLC) -i $^ > $(^:.ml=.mli)

all: libbvls.a

test: test.ml
	$(OCAMLC) -ccopt -L. bvls.cma test.ml

install: $(INSTALL_OBJ)
	$(INSTALL) $(INSTALL_OBJ) $(OCAML_DIR)

libbvls.a: $(CAML_OBJ) $(C_OBJ)
	$(OCAMLMKLIB) -o bvls $^

bvls-ps.c: bvls-ps.f
	$(F2C) bvls-ps.f

bvls.cmxa: libbvls.a bvls.cmi
bvls.cma: libbvls.a bvls.cmi
dllbvls.so: libbvls.a bvls.cmi

clean:
	rm -f *.o *.so *.cmxa *.cma *.cmx *.cmo *.cmi *.mli
	rm -f bvls.a libbvls.a
	rm -f bvls-ps.c
	rm -f a.out
