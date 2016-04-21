# The location of the ocaml source directory
OCAML_DIR=/usr/lib/ocaml

# The OCaml binaries
OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLMKLIB=ocamlmklib

INSTALL=install

# Enable debugging?
#DEBUG=-g
DEBUG=-DNDEBUG -O3

# The caml header files
INCLUDE=-I$(OCAML_DIR)

CFLAGS=$(DEBUG) $(INCLUDE) -fPIC -std=c99 -Wall -Werror
LDFLAGS=-lm

C_OBJ=				\
	debug.o			\
	hist_bins.o		\
	hist_points.o		\
	hist.o

CAML_OBJ=			\
	histogram_stubs.o	\
	histogram.cmx		\
	histogram.cmo

TEST_PROGS=			\
	test			\
	test_points		\
	test_bins		\
	test_convolve		\
	test_ml			\
	test_ml.opt

INSTALL_OBJ=			\
	histogram.cmi		\
	histogram.cma		\
	histogram.cmxa		\
	libhistogram.a		\
	histogram.a		\
	dllhistogram.so


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

all: libhistogram.a $(TEST_PROGS)

install: $(INSTALL_OBJ)
	$(INSTALL) $(INSTALL_OBJ) $(OCAML_DIR)

libhistogram.a: $(CAML_OBJ) $(C_OBJ)
	$(OCAMLMKLIB) -o histogram $^

histogram.cmxa: libhistogram.a histogram.cmi
histogram.cma: libhistogram.a histogram.cmi
dllhistogram.so: libhistogram.a histogram.cmi

test: test.c $(C_OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o test

test_points: test_points.c $(C_OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o test_points

test_bins: test_bins.c $(C_OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o test_bins

test_convolve: test_convolve.c $(C_OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o test_convolve

test_ml.opt : histogram.cmxa test.ml
	$(OCAMLOPT) -ccopt -L. $^ -o test_ml.opt

test_ml: histogram.cma test.ml
	$(OCAMLC) -ccopt -L. $^ -o test_ml

clean:
	rm -f *.o *.a *.so *.cmxa *.cma *.cmx *.cmo *.cmi *.mli
	rm -f $(TEST_PROGS)

