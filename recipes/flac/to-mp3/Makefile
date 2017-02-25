# ——— parameters ———————————————————————————————————————————————————————————————
bitrate = 320k
frequency = 48000
channels = 2

# ——— implementation ———————————————————————————————————————————————————————————
DIRFLAC = $(shell dir *.flac)
DIRMP3 = $(DIRFLAC:.flac=.mp3)

all: clean Makefile $(DIRMP3)

%.mp3: %.flac
	avconv -i "$<" -ab $(bitrate) -ac $(channels) -ar $(frequency) "$@"
	echo "$@" >> provide

clean:
	rm -f *.mp3 provide

install-tools:
	sudo apt-get install libav-tools

.PHONY: all install-tools clean
