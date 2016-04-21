# Makefile

#----------------------------------------------------

BINS=./group/bin/x86_64-linux_3.12.1/
#BINS=
DEV=~/research
UTILS = $(DEV)/wrutils
STRUCTS = $(DEV)/structs
OS=$(shell uname)
OCAMLC=$(BINS)ocamlc
OCAMLOPT=$(BINS)ocamlopt
PARSER=$(BINS)camlp4o
PARSER_INCLUDES=$(shell $(PARSER) -where)
INCLUDES = -I $(UTILS) -I $(STRUCTS) -I $(PARSER_INCLUDES)

# libraries:
#   str (for wr_utils/wrstring.ml)
#   unix (for wr_utils/wrio?)
LIBS = str.cma unix.cma
LIBSX = str.cmxa unix.cmxa


SRC =   $(UTILS)/wrutils.ml \
	$(UTILS)/verb.mli $(UTILS)/verb.ml \
	$(UTILS)/dset.mli $(UTILS)/dset.ml \
	$(UTILS)/eqvclass.mli $(UTILS)/eqvclass.ml \
	$(UTILS)/fn.ml \
	$(UTILS)/wrlist.ml \
	$(UTILS)/wrchar.ml \
	$(UTILS)/wrstr.ml \
	$(UTILS)/wrstrm.ml \
	$(UTILS)/fixedq.mli $(UTILS)/fixedq.ml \
	$(UTILS)/wrfname.ml \
	$(UTILS)/wrio.ml \
	$(UTILS)/wrsys.ml \
	$(UTILS)/wrht.ml \
	$(STRUCTS)/graph.mli $(STRUCTS)/graph.ml \
	$(STRUCTS)/dag.mli $(STRUCTS)/dag.ml \
	ocm.ml

default:
	@echo "Try 'make unix' or 'make byte', perhaps?"

byte:
	$(OCAMLC) -pp $(PARSER) -o ocm-byte $(INCLUDES) $(LIBS) -g $(SRC)

unix:
	$(OCAMLOPT) -pp $(PARSER) -o ocm-$(OS) $(INCLUDES) $(LIBSX) -g $(SRC)


clean:
	rm -f ocm-byte
	rm -f ocm-$(OS)
	rm -f *.cm[oxi]
	rm -f *.o
# EOF
