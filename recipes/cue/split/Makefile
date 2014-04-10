# ——— parameters ———————————————————————————————————————————————————————————————
FILEFORMAT = "`printf %02g $$TRACKNUMBER` - $$ARTIST - $$TITLE.flac"

# ——— implementation ———————————————————————————————————————————————————————————
remcontents = if [ -f $(1) ]; then cat $(1) | while read f; do rm -f "$$f";done;fi

CUEFILE = $(shell dir *.cue)

all: split provide

split: $(CUEFILE)
	make clean
	cuebreakpoints "$(CUEFILE)" | shnsplit -o flac *.flac
	cuetag "$(CUEFILE)" split-track*.flac
	ls split-track*.flac > "$@"

provide: split Makefile
	rm -f "$@"
	cat split | while read a; do \
	    ARTIST=`metaflac "$$a" --show-tag=ARTIST | sed s/.*=//g`; \
	    TITLE=`metaflac "$$a" --show-tag=TITLE | sed s/.*=//g`; \
	    TRACKNUMBER=`metaflac "$$a" --show-tag=TRACKNUMBER | sed s/.*=//g`; \
	    mv "$$a" $(FILEFORMAT); \
	    echo $(FILEFORMAT) >> "$@"; \
	done
	cp "$@" split

clean:
	if [ -f output ]; then $(call remcontents,output);rm -f output split;fi
	if [ -f split ]; then $(call remcontents,split);rm -f split;fi
	rm -f provide

install-tools:
	sudo apt-get install cuetools shntool flac wavpack

# List of the required files, besides the cue file.
#
# Note that it will require all flac files in the current directory,
# which isn't very smart, but certainly works when there's just one.
require:
	@for a in *.flac; do \
	echo "$$a"; \
	done

.PHONY: all install-tools require clean
