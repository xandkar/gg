EXE := gg
SRC := main.rkt
PREFIX := $(HOME)

$(EXE): $(SRC)
	raco exe --orig-exe -o $@ $(SRC)

.PHONY: build
build: $(EXE)

.PHONY: rebuild
rebuild:
	$(MAKE) --always-make build

.PHONY: install
install: $(EXE)
	mkdir -p $(PREFIX)/bin/
	cp $(EXE) $(PREFIX)/bin/

.PHONY: test
test:
	raco test *.rkt

.PHONY: smoke
smoke: $(EXE)
	./smoke-test

.PHONY: doc
doc:
	raco scribble --dest doc/html --dest-name index doc/gg.scrbl

.PHONY: clean
clean:
	rm -f $(EXE)
