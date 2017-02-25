# ——— parameters ———————————————————————————————————————————————————————————————
left   = 1
top    = 10
width  = 220
height = 220

# ——— implementation ———————————————————————————————————————————————————————————
INOGV = $(shell find . -maxdepth 1 -type f -name '*.ogv' ! -name '*.out.ogv' | tr '\n' ' ')
OUTOGV = $(INOGV:.ogv=.out.ogv)

all: clean $(OUTOGV)

%.out.ogv: %.ogv
	rm -f *.out.ogv
	ffmpeg -i "$<" -r 24 -mbd rd -flags +mv4+aic -trellis 2 -cmp 2 -subcmp 2 -g 300 -pass 1 \
	-y -f ogg -vf "crop=$(width):$(height):$(left):$(top)" /dev/null
	ffmpeg -i "$<" -r 24 -mbd rd -flags +mv4+aic -trellis 2 -cmp 2 -subcmp 2 -g 300 -pass 2 \
	-vf "crop=$(width):$(height):$(left):$(top)" "$@"
	echo "$@" >> provide

clean:
	rm -f *.out.ogv *.log provide

install-tools:
	sudo apt-get install ffmpeg

.PHONY: all install-tools clean
