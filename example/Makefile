HOST := $(shell hostname)
IGNORE_FILE := .gg-ignore-$(HOST)
IGNORE_FILE_EXAMPLE := .gg-ignore-HOSTNAME
SEARCH_TARGETS_FILE := .gg-targets-$(HOST)
SEARCH_TARGETS_FILE_EXAMPLE := .gg-targets-HOSTNAME

.PHONY: update
update: $(SEARCH_TARGETS_FILE) $(IGNORE_FILE)
	mkdir -p data
	gg --search --output-file data/"$(HOST)".rktd $(shell xargs < $(SEARCH_TARGETS_FILE))
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

$(SEARCH_TARGETS_FILE):
	cp $(SEARCH_TARGETS_FILE_EXAMPLE) $(SEARCH_TARGETS_FILE)

.PHONY: render
render: reports/host.$(HOST).png \
        reports/all.png

reports/host.$(HOST).png: reports/host.$(HOST).dot
	neato -Tpng -Goverlap=false -o $@ $<

reports/all.png: reports/all.dot
	neato -Tpng -Goverlap=false -o $@ $<
