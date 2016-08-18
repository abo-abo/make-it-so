# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)

# ______________________________________________________________________________

DIRMP4 = $(shell dir *.mp4)
DIRMP3 = $(shell dir *.mp3)

DIROUT.MP4 = $(DIRMP4:.mp4=.out.mp4)

all: clean Makefile $(DIROUT.MP4)

%.out.mp4: %.mp4
	avconv -i "$^" -i $(DIRMP3) \
	-c:v copy -map 0:0 -map 1:0 \
	-c:a libvo_aacenc -b:a 320k -ar 48000 -async 48000 \
	"$@"

clean:
	rm -f *.out.mp4 provide

# Insert the install command here.
# e.g. sudo apt-get install ffmpeg
install-tools:
	sudo apt-get install libavcodec-extra

# Use this target when one file requires another.
# See "../../cue/split/Makefile" for an example.
require:
	@echo

.PHONY: all install-tools require clean
