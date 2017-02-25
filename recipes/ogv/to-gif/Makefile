# ——— comments —————————————————————————————————————————————————————————————————
# The frames are stored in output/. You can
# - delete frames with e.g. dired
# - edit frames with e.g. GIMP
# Afterwards, call make again to compose the frames in output/ into a gif.

# ——— implementation ———————————————————————————————————————————————————————————
OGV = $(shell find . -maxdepth 1 -type f -name '*.ogv' ! -name '*.out.ogv' | tr '\n' ' ')
GIF = $(OGV:.ogv=.gif)

all: $(GIF)

%.output: %.ogv
	mplayer -ao null $< -vo jpeg:outdir="$@"

%.unoptimized.gif: %.output
	convert "$</*" "$@"

%.gif: %.unoptimized.gif
	convert "$^" -fuzz 10% -layers Optimize "$@"
	echo "$@" >> provide

clean:
	rm -f *.gif
	rm -rf ./*.output

install-tools:
	sudo apt-get install ffmpeg imagemagick

.PHONY: all install-tools provide clean
