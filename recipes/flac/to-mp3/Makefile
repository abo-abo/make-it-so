bitrate = 196k
frequency = 48000
channels = 2

all: output.mp3

output.mp3: in.flac
	ffmpeg -i $< -ab $(bitrate) -ac $(channels) -ar $(frequency) $@

clean:
	rm -f *.mp3

install-tools:
	sudo apt-get install ffmpeg

require:
	@echo

provide:
	@echo "output.mp3"

.PHONY: all install-tools require provide clean
