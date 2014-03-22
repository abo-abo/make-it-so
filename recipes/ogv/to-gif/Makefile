
# ———  —————————————————————————————————————————————————————————————————————————
OGV = $(shell find . -maxdepth 1 -type f -name '*.ogv' ! -name '*.out.ogv' | tr '\n' ' ')
GIF = $(OGV:.ogv=.gif)

all: $(GIF)

%.output: %.ogv
	mplayer -ao null $< -vo jpeg:outdir=$@

%.unoptimized.gif: %.output
	convert $</* $@

%.gif: %.unoptimized.gif
	convert $^ -fuzz 10% -layers Optimize $@
	echo $@ >> provide

clean:
	rm -f *.gif
	rm -rf ./*.output

install-tools:
	sudo apt-get install ffmpeg imagemagick

require:
	@echo

.PHONY: all install-tools require provide clean
