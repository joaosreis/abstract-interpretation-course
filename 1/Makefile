# Cours "Semantics and applications to verification"
#
# Marc Chevalier 2018
# Ecole normale supérieure, Paris, France / CNRS / INRIA

# Feel free to change it
RESULT = TP1.byte
OCAMLYACC = menhir
YFLAGS = --explain --table
SOURCES = \
	libs/mapext.ml libs/mapext.mli \
	libs/uniqueId.ml libs/option.ml libs/fn.ml \
	frontend/abstract_syntax_tree.ml \
	frontend/abstract_syntax_printer.ml frontend/abstract_syntax_printer.mli \
	frontend/parser.mly frontend/lexer.mll \
	frontend/parser_messages.ml \
	frontend/file_parser.ml frontend/file_parser.mli \
	frontend/interpreter.ml frontend/n_interpreter.ml \
	main.ml

PACKS = zarith menhirLib
LIBS = zarith
OCAMLBLDFLAGS = -I $(shell ocamlfind query zarith) -I $(shell ocamlfind query menhirLib) -g
OCAMLFLAGS = -w +a-4-9 -g

all: messages byte-code

messages: frontend/parser.messages
	$(OCAMLYACC) $(YFLAGS) frontend/parser.mly --compile-errors frontend/parser.messages > frontend/parser_messages.ml

.PHONY: messages

OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
