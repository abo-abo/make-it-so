;;; make-it-so.el --- Transform files with Makefile recipes. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/make-it-so
;; Version: 0.1.0
;; Package-Requires: ((helm "1.5.3"))
;; Keywords: make, dired

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package is aimed on minimizing the effort of interacting with
;; the command tools involved in trasforming files.
;;
;; For instance, once in a blue moon you might need to transform a
;; large flac file based on a cue file into smaller flac files, or mp3
;; files for that matter.
;;
;; Case 1: if you're doing it for the first time, you'll search the
;; internet for a command tool that does this job, and for particular
;; switches.
;;
;; Case 2: you've done it before.  The problem is that when you want to
;; do the transform for the second time, you have likely forgotten all
;; the command switches and maybe even the command itself.
;;
;; The solution is to write the command to a Makefile when you find it
;; the first time around.  This particular Makefile you should save to
;; ./recipes/cue/split/Makefile.
;;
;; Case 3: you have a Makefile recipe.  Just navigate to the dispatch
;; file (*.cue in this case) with `dired' and press "," which is bound
;; to `make-it-so'.  You'll be prompted to select a transformation
;; recipe that applies to *.cue files.  Select "split".  The following
;; steps are:
;;
;;   1. A special directory will be created in place of the input
;;   files and they will be moved there.
;;
;;   2. Your selected Makefile template will be copied to the special
;;   dir and opened for you to tweak the parameters.
;;
;;   3. When you're done, call `compile' to make the transformation.
;;
;;   4. If you want to cancel at this point, discarding the results of
;;   the transformation (which is completely safe, since they can be
;;   regenerated), call `mis-abort'.
;;
;;   5. If you want to keep both the input and output files, call
;;   `mis-finalize'
;;
;;   6. If you want to keep only the output files, call `mis-replace'.
;;
;;   7. Finally, consider contributing Makefile recipies to allow
;;   other users to skip step 1.
;;

;;; Code:

;; ——— Requires ————————————————————————————————————————————————————————————————
(require 'helm)
(require 'dired)
(require 'make-mode)
(require 'cl-lib)

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup make-it-so nil
  "Transfrom files in `dired' with Makefile recipes."
  :group 'dired
  :prefix "mis-")

(defcustom mis-recipes-directory "~/git/make-it-so/recipes/"
  "Directory with available recipes."
  :type 'directory
  :group 'make-it-so)

(defcustom mis-bindings-alist
  '((make-it-so . ",")
    (mis-finalize . "C-,")
    (mis-abort . "C-M-,")
    (mis-dispatch . "C-.")
    (mis-replace . "C-M-."))
  "List of bindings for the minor mode."
  :group 'make-it-so)

(defcustom mis-make-command "make -j8"
  "Customize the make command bound to `mis-make-key'.
Option -j8 will allow up to 8 asynchronous processes to make the targets."
  :group 'make-it-so)

(defcustom mis-make-key "<f5>"
  "`mis-make-command' will be bound to this key in `makefile-mode'."
  :group 'make-it-so)

;; ——— Setup ———————————————————————————————————————————————————————————————————
(defvar mis-mode-map
  (make-sparse-keymap))

(let ((map mis-mode-map))
  (mapc (lambda (x) (define-key map (kbd (cdr x)) (car x)))
        mis-bindings-alist))

(defvar mis-current-files nil
  "Current set of files to transform.")

;;;###autoload
(define-minor-mode mis-mode
    "Add make-it-so key bindings to `dired'.

\\{mis-mode-map}"
  :keymap mis-mode-map
  :group 'make-it-so)

;;;###autoload
(defun mis-mode-on ()
  "Enable make-it-so bindings."
  (mis-mode 1))

;;;###autoload
(defun mis-config-default ()
  "Easy config."
  (add-hook 'dired-mode-hook 'mis-mode-on)
  (when mis-make-key
    (define-key makefile-mode-map (kbd mis-make-key) 'mis-save-and-compile)))

;; ——— Interactive —————————————————————————————————————————————————————————————
;;;###autoload
(defun mis-browse ()
  "List all available recipes.
Jump to the Makefile of the selected recipe."
  (interactive)
  (helm :sources
        `((name . "Recipes")
          (candidates . mis-recipes)
          (action . (lambda (x)
                      (if (string-match "^\\([^-]+\\)-\\(.*\\)$" x)
                          (find-file
                           (mis-build-path
                            mis-recipes-directory
                            (match-string 1 x)
                            (match-string 2 x)
                            "Makefile"))
                        (error "Failed to split %s" x)))))))

(defun mis-create-makefile (action)
  (let* ((olde (file-name-extension (car mis-current-files)))
         (newe (if (string-match "^to-\\(.*\\)" action)
                   (match-string 1 action)
                 (concat "out." olde)))
         (preamble (concat "# This is a template for the Makefile.\n"
                           "# Parameters should go in the upper half as:\n"
                           "#     width = 200\n"
                           "# and be referenced in the command as $(width)\n\n"
                           "# " (make-string 78 ?_)))
         (olds (format "DIR%s = $(shell dir *.%s)" (upcase olde) olde))
         (news (format "DIR%s = $(DIR%s:.%s=.%s)"
                       (upcase newe) (upcase olde) olde newe))
         (t-all (format "all: clean Makefile $(DIR%s)" (upcase newe)))
         (t-new (concat
                 (format "%%.%s: %%.%s\n\techo \"add command here\"" newe olde)
                 "\n\techo $@ >> provide"))
         (t-clean (format "clean:\n\trm -f *.%s provide" newe))
         (t-tools (concat
                   "# Insert the install command here.\n"
                   "# e.g. sudo apt-get install ffmpeg\n"
                   "install-tools:\n\t"
                   "echo \"No tools required\""))
         (t-req (concat
                 "# Use this target when one file requires another.\n"
                 "# See \"../../cue/split/Makefile\" for an example.\n"
                 "require:\n\t@echo"))
         (t-phony ".PHONY: all install-tools require clean")
         (Makefile (mapconcat 'identity
                              (list preamble olds news t-all t-new
                                    t-clean t-tools t-req t-phony)
                              "\n\n")))
    (mis-spit Makefile
              (mis-build-path-create
               mis-recipes-directory
               olde
               action
               "Makefile"))))

;;;###autoload
(defun make-it-so ()
  "When called from `dired', offer a list of transformations.
Available trasformations are dispatched on currently selected
file(s)' extension. Therefore it's an error when files with
multiple extensions are marked.  After an action is selected,
proceed to call `mis-action' for that action."
  (interactive)
  (unless (eq major-mode 'dired-mode)
    (error "Must be called from dired"))
  (if (mis-all-equal
       (mapcar
        #'file-name-extension
        (setq mis-current-files
              (dired-get-marked-files nil current-prefix-arg))))
      (let* ((ext (file-name-extension (car mis-current-files)))
             (candidates (mis-recipes-by-ext ext))
             (source1 `((name . "Makefiles")
                        (candidates . ,candidates)
                        (action . mis-action)))
             (source2 `((name . "Create Makefile")
                        (dummy)
                        (action . mis-action))))
        (helm :sources
              (list source1 source2)))
    (error "Mixed extensions in selection")))

;;;###autoload
(defun mis-abort ()
  "Abort tranformation.
This function should revert to the state before `mis-action' was called."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (let ((makefile-buffer (find-buffer-visiting (expand-file-name "Makefile")))
        (dired-buffer (current-buffer))
        (dir default-directory)
        (targets (read (mis-slurp "targets")))
        (sources (read (mis-slurp "sources"))))
    (when makefile-buffer
      (kill-buffer makefile-buffer))
    (cl-mapcar 'rename-file targets sources)
    (dired "..")
    (kill-buffer dired-buffer)
    (delete-directory dir t)
    (revert-buffer)))

;;;###autoload
(defun mis-finalize ()
  "Finalize transformation.
In addition to `mis-abort' copy over the files listed in
\"provide\".  Each Makefile should append all essential files
that it creates to a \"provide\" file. All generated files not in \"provide\"
(intermediates and logs and such) will be deleted."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (unless (file-exists-p "provide")
    (error "No provide in current directory"))
  (let ((provides (split-string (mis-slurp "provide") "\n" t)))
    (mapc (lambda (f) (mis-rename-unquote f (expand-file-name ".."))) provides)
    (mis-abort)))

;;;###autoload
(defun mis-replace ()
  "Finalize transformation.
In addition to `mis-finalize' move source files to trash."
  (interactive)
  (let ((sources (read (mis-slurp "sources"))))
    (mis-finalize)
    (mapc 'mis-delete-file sources))
  (revert-buffer))

;;;###autoload
(defun mis-dispatch ()
  "Choose \"mis-\" via `helm'."
  (interactive)
  (helm :sources
        `((name . "Actions")
          (candidates .
                      ,(mapcar
                        (lambda (x)
                          (list
                           (format "% -30s%s" x
                                   (or (cdr (assoc x mis-bindings-alist))
                                       "not bound"))
                           x))
                        '(mis-finalize mis-abort mis-replace)))
          (action . (lambda (x) (call-interactively (car x)))))))

;;;###autoload
(defun mis-save-and-compile ()
  "Save current buffer and call `compile' with `mis-make-command'.
Switch to other window afterwards."
  (interactive)
  (save-buffer)
  (compile mis-make-command))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun mis-directory-files (directory)
  "Return results of (`directory-files' DIRECTORY) without \".\" and \"..\"."
  (and (file-exists-p directory)
       (delete "." (delete ".." (directory-files directory)))))

(defun mis-recipes-by-ext (ext)
  "Return a list of recipes available for EXT."
  (mis-directory-files
   (expand-file-name ext mis-recipes-directory)))

(defun mis-recipes ()
  "Return a list of current recipes."
  (let ((formats (mis-directory-files mis-recipes-directory)))
    (apply #'append
           (cl-loop for f in formats
                    collect
                    (mapcar (lambda (x) (format "%s-%s" f x))
                            (mis-recipes-by-ext f))))))

(defun mis-build-path (&rest lst)
  "Build a path from LST."
  (cl-reduce (lambda (a b) (expand-file-name b a)) lst))

(defun mis-build-path-create (&rest lst)
  "Build a path from LST. Create intermediate directories."
  (expand-file-name
   (car (last lst))
   (cl-reduce
    (lambda (a b)
      (let ((dir (expand-file-name b a)))
        (unless (file-exists-p dir)
          (make-directory dir))
        dir))
    (butlast lst 1))))

(defun mis-slurp (file)
  "Return contents of FILE."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-substring-no-properties
         (point-min)
         (point-max)))
    (error "No file \"%s\" in current directory" file)))

(defun mis-spit (str file)
  "Write STR to FILE."
  (with-temp-buffer
    (insert str)
    (write-region nil nil file nil 1)))

(defun mis-all-equal (lst)
  "Return t if all elements of LST are equal."
  (cl-every (lambda (x) (equal x (car lst)))
            (cdr lst)))

(defun mis-action (x)
  "Make it so for recipe X."
  (let* ((sources mis-current-files)
         (source (file-name-nondirectory (car sources)))
         (ext (file-name-extension source))
         (basedir (or (file-name-directory source)
                      default-directory))
         (dir (expand-file-name
               (format "%s:%s" x (file-name-nondirectory source))
               basedir))
         (makefile-template
          (mis-build-path mis-recipes-directory ext x "Makefile"))
         (makefile (expand-file-name "Makefile" basedir)))
    ;; If a recipe exists, copy it.
    ;; Otherwise create a new one, move it here and mark it to be
    ;; restored to the proper location.
    (if (file-exists-p makefile-template)
        (copy-file makefile-template makefile)
      (mis-create-makefile x)
      (push makefile-template sources))
    (if (file-exists-p "Makefile")
        (let ((requires (shell-command-to-string "make require")))
          (if (string-match "make:" requires)
              (error "Makefile must have a \"require\" target")
            (mkdir dir)
            (rename-file "Makefile" dir)
            (setq sources
                  (append sources
                          (mapcar 'expand-file-name
                                  (split-string requires "\n" t))))))
      (mkdir dir))
    (let ((targets (mapcar (lambda (x) (mis-rename-quote x dir))
                           sources)))
      (mis-spit (prin1-to-string sources)
                (expand-file-name "sources" dir))
      (mis-spit (prin1-to-string targets)
                (expand-file-name "targets" dir)))
    (find-file (expand-file-name "Makefile" dir))))

(defun mis-delete-file (file)
  "Delete FILE."
  (move-file-to-trash file))

(defun mis-rename-quote (file dir)
  "Move FILE to DIR, changing spaces to underscores."
  (let ((dest (expand-file-name
               (replace-regexp-in-string
                " " "_" (file-name-nondirectory file)) dir)))
    (rename-file file dest)
    dest))

(defun mis-rename-unquote (file dir)
  "Move FILE to DIR, changing spaces to underscores."
  (let ((dest (expand-file-name
               (replace-regexp-in-string
                "_" " " (file-name-nondirectory file))
               dir)))
    (rename-file file dest)
    dest))

(provide 'make-it-so)

;;; Local Variables:
;;; outline-regexp: ";; ———"
;;; End:

;;; make-it-so.el ends here
