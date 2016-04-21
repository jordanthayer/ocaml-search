#!/bin/sh
#
# Installs the object files into the ocaml directory.
#

OCAML_DIR=""

echo installing to $OCAML_DIR

install _build/playerc.cmxa $OCAML_DIR
install _build/playerc.cma $OCAML_DIR
install _build/playerc.cmi $OCAML_DIR
install _build/playerc.o $OCAML_DIR
install _build/playerc.a $OCAML_DIR
install _build/libplayerc_caml.a $OCAML_DIR
install _build/dllplayerc_caml.so $OCAML_DIR
