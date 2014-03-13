BIN=main.native

all:
	ocamlbuild $(BIN)
	cp _build/$(BIN) guru

clean:
	rm -rf main.native _build guru


