SRC=Type.ml Parser.ml Lexer.ml
ITF=Parser.mli
OBJ=$(SRC:%.ml=%.cmo)

all: $(OBJ)

run: all
	ocaml Type.cmo Parser.cmo Lexer.cmo Main.ml

$(OBJ): $(SRC) $(ITF)
	ocamlc -c $(ITF) $(SRC)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll

clean:
	rm *.cmo *.cmi Parser.ml Lexer.ml Parser.mli

.PHONY: all run clean
