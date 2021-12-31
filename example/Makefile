HOST := $(shell hostname)
IGNORE_FILE := .gg-ignore-$(HOST)
IGNORE_FILE_EXAMPLE := .gg-ignore-HOSTNAME

.PHONY: update
update: $(IGNORE_FILE)
	mkdir -p data
	gg --search --output-file data/"$(HOST)".rktd ~
	mkdir -p reports
	gg --read --report-graph --output-file reports/host."$(HOST)".dot data/"$(HOST)".rktd
	gg --read --report-graph --output-file reports/all.dot data/*.rktd
	rm -rf indices	# XXX So that git can detect file renames/deletions for us automatically.
	mkdir -p indices
	gg --read --dir-tree indices data/*.rktd

.PHONY: commit
commit:
	git add data reports indices
	git commit -m "Update from $(HOST)"

$(IGNORE_FILE):
	cp $(IGNORE_FILE_EXAMPLE) $(IGNORE_FILE)
