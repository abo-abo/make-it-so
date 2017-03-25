# ——— parameters ———————————————————————————————————————————————————————————————
LANGUAGE = zh_CN
ENCODE_TO = utf-8

# ——— implementation ———————————————————————————————————————————————————————————
DIRMIX = $(shell dir *.mix)
DIROUT = $(DIRMIX:.mix=.out)

all: $(DIROUT)

%.out: %.mix
	cp "$^" "$@"
	enca -L $(LANGUAGE) -x $(ENCODE_TO) "$@"
	echo "$@" >> provide

install-tools:
	sudo apt-get install enca

.PHONY: all install-tools
