from = 00:00:01
to   = 00:00:23
# ______________________________________________________________________________
INOGV = $(shell find . -maxdepth 1 -name '*.ogv' -type f ! -name '*.out.ogv' | tr '\n' ' ')
OUTOGV = $(INOGV:.ogv=.out.ogv)

all: $(OUTOGV)

%.out.ogv: %.ogv Makefile
	rm -f $@
	ffmpeg -i $< -ss $(from) -t $(to) -c copy $@
	echo $@ >> provide

clean:
	rm -f *.out.ogv provide

install-tools:
	sudo apt-get install ffmpeg

require:
	@echo

.PHONY: all install-tools require clean
