# ——— parameters ———————————————————————————————————————————————————————————————
from = 00:00:01
to   = 00:00:23

# ——— implementation ———————————————————————————————————————————————————————————
INOGV = $(shell find . -maxdepth 1 -name '*.ogv' -type f ! -name '*.out.ogv' | tr '\n' ' ')
OUTOGV = $(INOGV:.ogv=.out.ogv)

all: $(OUTOGV)

%.out.ogv: %.ogv Makefile
	rm -f "$@"
	ffmpeg -i $< -ss $(from) -t $(to) -vcodec copy -acodec copy "$@"
	echo "$@" >> provide

clean:
	rm -f *.out.ogv provide

install-tools:
	sudo apt-get install ffmpeg

.PHONY: all install-tools clean
