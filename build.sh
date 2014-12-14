#! /bin/sh

ocamlbuild \
  -use-ocamlfind \
  -syntax camlp4o \
  -pkg sexplib.syntax,comparelib.syntax,fieldslib.syntax,variantslib.syntax \
  -pkg bin_prot.syntax \
  -tag bin_annot \
  -tag annot \
  -tag debug \
  -tag short_paths \
  -cflags "-w A-4-33-40-41-42-43-34-44-48" \
  -cflags -strict-sequence \
  -j 5 \
  -use-menhir \
  -pkg cmdliner \
  -pkg core_kernel \
  -pkg fileutils \
  $@
