bitrate = 196k
frequency = 48000
channels = 2

# ———  —————————————————————————————————————————————————————————————————————————
DIRFLAC = $(shell dir *.flac)
DIRMP3 = $(DIRFLAC:.flac=.mp3)

all: $(DIRMP3)

%.mp3: %.flac
	ffmpeg -i $< -ab $(bitrate) -ac $(channels) -ar $(frequency) $@

clean:
	rm -f *.mp3

provide:
	@echo $(wildcard *.mp3)

require:
	@echo

install-tools:
	sudo apt-get install ffmpeg

.PHONY: all install-tools require provide clean
