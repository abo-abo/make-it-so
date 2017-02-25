# ——— parameters ———————————————————————————————————————————————————————————————
bitrate = 320k

# ——— implementation ———————————————————————————————————————————————————————————
DIRMP4 = $(shell dir *.mp4)
DIRMP3 = $(DIRMP4:.mp4=.mp3)

all: clean Makefile $(DIRMP3)

%.mp3: %.mp4
	AV_LOG_FORCE_NOCOLOR=TRUE avconv -loglevel info -i "$<" -vn -ab $(bitrate) "$@"
	echo "$@" >> provide

clean:
	rm -f *.mp3 provide

install-tools:
	sudo apt-get install libav-tools libavcodec-extra-53

.PHONY: all install-tools clean
