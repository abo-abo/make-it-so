from = 00:00:01
to   = 00:00:23

all: out.ogv

out.ogv: in.ogv Makefile
	rm -f $@
	ffmpeg -i $< -ss $(from) -t $(to) -c copy $@

install-tools:
	sudo apt-get install ffmpeg

require:
	@echo

provide:
	@echo

.PHONY: all install-tools require provide clean
