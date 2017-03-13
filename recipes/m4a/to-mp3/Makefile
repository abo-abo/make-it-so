DIRM4A = $(shell dir *.m4a)

DIRMP3 = $(DIRM4A:.m4a=.mp3)

all: clean Makefile $(DIRMP3)

%.mp3: %.m4a
	ffmpeg -v 5 -y -i "$^" -acodec libmp3lame -ac 2 -ab 192k "$@"
	echo "$@" >> provide

clean:
	rm -f *.mp3 provide

install-tools:
	sudo apt-get install ffmpeg

.PHONY: all install-tools clean
