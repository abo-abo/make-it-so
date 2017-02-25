EMACS = emacs

.PHONY: compile clean

compile:
	$(EMACS) -batch  --eval "(byte-compile-file \"make-it-so.el\")"

clean:
	rm -f *.elc
