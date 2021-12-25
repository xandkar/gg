EXE := git-locations

$(EXE): $(EXE).rkt
	raco exe --orig-exe -o $@ $(EXE).rkt

.PHONY: build
build: $(EXE)

.PHONY: rebuild
rebuild:
	$(MAKE) --always-make build

.PHONY: test
test:
	raco test *.rkt

.PHONY: smoke
smoke: $(EXE)
	./smoke-test

.PHONY: clean
clean:
	rm -f $(EXE)
