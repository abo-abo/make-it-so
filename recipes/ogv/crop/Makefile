left   = 1
top    = 10
width  = 950
height = 715

all: out.ogv

out.ogv: in.ogv Makefile ffmpeg2pass-0.log
	rm -f $@
	ffmpeg -i $< \
	-r 24 \
	-mbd rd -flags +mv4+aic -trellis 2 -cmp 2 -subcmp 2 -g 300 -pass 2 \
	-vf "crop=$(width):$(height):$(left):$(top)" $@

ffmpeg2pass-0.log: in.ogv
	rm -f $@
	ffmpeg -i $< \
	-r 24 \
	-mbd rd -flags +mv4+aic -trellis 2 -cmp 2 -subcmp 2 -g 300 -pass 1 \
	-vf "crop=$(width):$(height):$(left):$(top)" out.ogv

clean:
	rm -f out.ogv ffmpeg2pass-0.log

install-tools:
	sudo apt-get install ffmpeg

require:
	@echo

provide:
	@echo

.PHONY: all install-tools require provide clean


