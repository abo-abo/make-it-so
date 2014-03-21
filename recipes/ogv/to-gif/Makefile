all: output.gif

output: in.ogv Makefile
	mplayer -ao null $< -vo jpeg:outdir=output

unoptimized.gif: output
	convert output/* unoptimized.gif

output.gif: unoptimized.gif
	convert $^ -fuzz 10% -layers Optimize $@

clean:
	rm -f *.gif
	rm -rf ./output

install-tools:
	sudo apt-get install ffmpeg imagemagick

require:
	@echo

provide:
	@echo "output.gif"

.PHONY: all install-tools require provide clean
