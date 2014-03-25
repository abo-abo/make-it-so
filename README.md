# make-it-so

**GNU Emacs package for transforming files with Makefile recipes**

## What and why

### Terse description

Curry command line tools with Makefiles and select Makefiles to call
from `dired` dispatching on selected files' extension.

### Long story

I'm sure you've encountered your share of "How do I transform X to
Y?"-type questions on the web. I know I have. A lot of answers involve
using kitchen-sink tools like `ffmpeg` or `convert`. These tools are
great: they can transform almost anything into anything else.  But
they come with a price of a gazillion switches that are hard to
remember (and to type in, for that matter).

This package solves this problem by moving the complex command to a
Makefile with customizable literate parameters at the top.
Additionally it catalogs all available Makefiles by the file extension
that they work on.
It also creates a Makefile template for you when a selected action
doesn't exist.

### Advantages

1. You write the command only once. All the other times, you just
   customize literate switches.

2. It's possible to not have to write the command at all if it's
   available in the repository.

3. Since the command is meant to be written just once, you can have a
   much more complex and optimized command, compared to the one that
   you would normally enter in the terminal.

4. In case of multiple files, `make` allows to use parallel processors
   without the actual command knowing about it. This means that people
   with different processors can use the same Makefile to the most efficiency,
   calling `make -j8` or `make -j2` where appropriate.

5. The ELisp layer of this package solves the problem of Makefiles
   and file names with spaces.

## Installation

Clone this repository:

    $ cd ~/git/
    $ git clone https://github.com/abo-abo/make-it-so

Add to `.emacs`:

    (add-to-list 'load-path "~/git/make-it-so")
    (require 'make-it-so)
    (setq mis-recipes-directory "~/git/make-it-so/recipes/")
    (mis-config-default)


## The workflow for using existing recipes

As a sample scenario, assume you want to trim (change running time)
one or more *.ogv files.

So you navigate to their location with `dired` and [mark][dired-mark]
them in case there's more than one.

Next, you call `make-it-so` (bound to **,**). It recognizes the "ogv"
extension and offers `crop`, `to-gif` and `trim` actions. You can
navigate with **C-n**/**C-p**, select with **RET** and cancel with **C-g**.

After you've selected `trim`, the selected files and the corresponding
[Makefile][trim-makefile] will be moved to a *staging* directory within
your current directory. The staging directory will be named after the
action and the first selected file.

The Makefile will be opened for you to customize switches which start out as:

    from = 00:00:01
    to   = 00:00:23

The actual command called will be:

    ffmpeg -i $< -ss $(from) -t $(to) -c copy $@

but this complexity is distanced from you, and you can just select
`from` and `to` if you want.

Next you call `mis-save-and-compile` (bound to "f5" for `make-mode`)
and test your result.

After this you can:

1. Call `mis-abort` (bound to **C-M-,**): the staging directory will be deleted and all
   files restored to their places. Basically, this is just an undo
   with respect to `make-it-so`.
2. Call `mis-finalize` (bound to **C-,**): in addition to `mis-abort` the generated files will be
   moved to the original directory.
3. Call `mis-replace` (bound to **C-M-.**): in addition to `mis-finalize` the source files will be
   moved to trash.

Note that in all three cases any modifications to the Makefile
template *will be lost*, along with all files in the staging directory
except those that were copied when you first called `make-it-so` and
those registered by the Makefile in "provide" file.

There's also *mis-dispatch* (bound to **C-.**) that allows to call the
above three commands by name and remind you their shortcuts.

## Available recipes

1. cue-spit
2. flac-to-mp3
3. ogv-trim
4. ogv-crop
5. ogv-to-gif
6. svg-to-png

## The workflow for adding new recipes

As a sample scenario, assume you want to convert *.svg to *.png.

1. An internet search lead to [Stack Overflow][stack] and this command:

        inkscape -z -e test.png -w 1024 -h 1024 test.svg

2. Navigate to the file(s) in `dired` and call `make-it-so` with **,**.
No default actions are available, so just type "to-png" and hit **RET**.
The "to-" prefix signifies that this is a conversion, adapting the Makefile to this form:

        # This is a template for the Makefile.
        # Parameters should go in the upper half as:
        #     width = 200
        # and be referenced in the command as $(width)

        # ______________________________________________________________________________

        DIRSVG = $(shell dir *.svg)

        DIRPNG = $(DIRSVG:.svg=.png)

        all: clean Makefile $(DIRPNG)

        %.png: %.svg
        	echo "add command here"
            echo $@ >> provide

        clean:
        	rm -f *.png provide

        # Insert the install command here.
        # e.g. sudo apt-get install ffmpeg
        install-tools:
        	echo "No tools required"

        # Use this target when one file requires another.
        # See "../../cue/split/Makefile" for an example.
        require:
        	@echo

        .PHONY: all install-tools require clean

3. In case the command name and package name don't coincide, or the
command needs additional packages in order to work you might want to
change `echo "No tools required"` to `sudo apt-get install inkscape`.

4. Replace `echo "add command here"` with `inkscape -z -e $@ -w $(width) -h $(height) $^`.
The parameters `width` and `height` will go to the top of the Makefile, where they
can be customized. `$@` refers to the output file, `test.png` in this case.
`$^` refers to the input file, `test.svg` in this case.

5. That's it. You can see the final Makefile [here][to-png-makefile].
Test if the command works with **f5** from the Makefile.  If you're
happy with it, call `mis-finalize` with **C-,** from `dired`.  The
Makefile will be saved for all future calls to `make-it-so`.

## Usage Tips

- Remember to use `dired-jump` command to jump from any file (or even
  the compilation buffer) to the associated directory.
- Remember `dired-mark-files-regexp` (bound to Shift 85) that will mark all files
  of specific type in directory. This is much faster than **m** when you want
  to convert a bunch of files.
- Until you're sure that the command and the Makefile work properly
  make backups. In fact, make backups period.

## Contributing

As you can see, the list of Makefile recipes is quite scarce at the
moment.  So new Makefile contributions are most welcome.

[dired-mark]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Marks-vs-Flags.html#Marks-vs-Flags
[trim-makefile]: https://raw.githubusercontent.com/abo-abo/make-it-so/master/recipes/ogv/trim/Makefile
[to-png-makefile]: https://raw.githubusercontent.com/abo-abo/make-it-so/master/recipes/svg/to-png/Makefile
[stack]: http://stackoverflow.com/questions/9853325/how-to-convert-a-svg-to-a-png-with-image-magick
