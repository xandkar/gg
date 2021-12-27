EXE := git-sonar
PREFIX := $(HOME)

$(EXE): $(EXE).rkt
	raco exe --orig-exe -o $@ $(EXE).rkt

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

.PHONY: clean
clean:
	rm -f $(EXE)
