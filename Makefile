SRC=Type.ml Parser.mli Parser.ml Lexer.ml
OBJ=$(patsubst %.mli,%.cmi,$(SRC:%.ml=%.cmo))

all: $(OBJ)

run: all
	ocaml Type.cmo Parser.cmo Lexer.cmo Main.ml code.lus -i in.data

compile : all
	ocamlc -c Main.ml
	ocamlc Type.cmo Parser.cmo Lexer.cmo Main.cmo -o lustre

$(OBJ): $(SRC)
	ocamlc -c $(SRC)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

clean:
	rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli

.PHONY: all run clean
