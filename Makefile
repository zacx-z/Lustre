OBJ_SRC=Type.ml Parser.mli Parser.ml Lexer.ml

all: objects

run: all
	ocaml Type.cmo Parser.cmo Lexer.cmo Main.ml

objects: $(OBJ_SRC)
	ocamlc -c $(OBJ_SRC)

Parser.ml: Parser.mly
	ocamlyacc Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

.PHONY: all run
