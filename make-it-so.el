;;; make-it-so.el --- Transform files with Makefile recipes. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/make-it-so
;; Version: 0.1.0
;; Package-Requires: ((swiper "0.8.0") (emacs "24"))
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
;; the command tools involved in transforming files.
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
;;   1. A staging directory will be created in place of the input
;;   files and they will be moved there.
;;
;;   2. Your selected Makefile template will be copied to the staging
;;   directory and opened for you to tweak the parameters.
;;
;;   3. When you're done, call `compile' to make the transformation.
;;   It's bound to [f5] in `make-mode' by this package.
;;
;;   4. If you want to cancel at this point, discarding the results of
;;   the transformation (which is completely safe, since they can be
;;   regenerated), call `mis-abort', bound to "C-M-,".
;;
;;   5. If you want to keep both the input and output files, call
;;   `mis-finalize', bound to "C-,".
;;
;;   6. If you want to keep only the output files, call `mis-replace',
;;   bound to "C-M-.".  The original files will be moved to trash.
;;
;;   7. Finally, consider contributing Makefile recipes to allow
;;   other users to skip Case 1 and Case 2.
;;

;;; Code:

;;* Requires
(require 'dired)
(require 'make-mode)
(require 'cl-lib)
(require 'package)

;;* Customization
(defgroup make-it-so nil
  "Transfrom files in `dired' with Makefile recipes."
  :group 'dired
  :prefix "mis-")

(defcustom mis-completion-method 'ivy
  "Method to select a candidate from a list of strings."
  :type '(choice
          (const :tag "Ivy" ivy)
          (const :tag "Helm" helm)))

(defvar mis-load-file-name (or load-file-name
                               (buffer-file-name)))

(defcustom mis-recipes-directory (expand-file-name
                                  "recipes"
                                  (file-name-directory mis-load-file-name))
  "Directory with available recipes."
  :type 'directory)

(defcustom mis-bindings-alist
  '((make-it-so . ",")
    (mis-finalize . "C-,")
    (mis-abort . "C-M-,")
    (mis-dispatch . "C-.")
    (mis-replace . "C-M-."))
  "List of bindings for the minor mode.")

(defcustom mis-make-command "make -j8"
  "Customize the make command.
Option -j8 will allow up to 8 asynchronous processes to make the targets.")

(defcustom mis-makefile-preamble "
# This is a template for the Makefile.
# Parameters should go in the upper half as:
#     width = 200
# and be referenced in the command as $(width)"
  "Preamble to be inserted at the top of makefile templates."
  :type 'string)

(defcustom mis-makefile-key-descriptions "
# Press <f5> (mis-save-and-compile) to run this makefile (i.e. apply the transformation).

# Then press one of:
# C-,   (mis-finalize)  : finalize the transformation (delete makefile and other auxiliary files).
# C-M-, (mis-abort)     : revert back to state before `mis-action' was called.
# C-M-. (mis-replace)   : additionally to finalizing, delete the original file."
  "Key descriptions inserted into Makefiles."
  :type 'string)

;;* Setup
(defvar mis-mode-map
  (let ((map (make-sparse-keymap)))
    (mapc (lambda (x) (define-key map (kbd (cdr x)) (car x)))
          mis-bindings-alist)
    map))

(defvar mis-makefile-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<f5>") 'mis-save-and-compile)
    (define-key map (kbd "C-,") 'mis-finalize)
    (define-key map (kbd "C-M-,") 'mis-abort)
    map))

(defvar mis-current-files nil
  "Current set of files to transform.")

;;;###autoload
(define-minor-mode mis-mode
  "Add make-it-so key bindings to `dired-mode'.

\\{mis-mode-map}"
  :keymap mis-mode-map)

;;;###autoload
(define-minor-mode mis-makefile-mode
  "Add make-it-so key bindings to `makefile-mode'

\\{mis-makefile-mode-map}"
  :keymap mis-makefile-mode-map)

;;;###autoload
(defun mis-config-default ()
  "Easy config."
  (add-hook 'dired-mode-hook 'mis-mode))

(declare-function helm "helm")
(declare-function ivy-read "ivy")

(defun mis-competing-read (prompt collection action)
  (if (eq mis-completion-method 'helm)
      (progn
        (require 'helm)
        (require 'helm-help)
        (helm :sources
              `((name . ,prompt)
                (candidates . ,collection)
                (action . ,action))))
    (require 'ivy)
    (ivy-read prompt collection
              :action action)))

;;* Interactive
;;;###autoload
(defun mis-browse ()
  "List all available recipes.
Jump to the Makefile of the selected recipe."
  (interactive)
  (mis-competing-read
   "Recipes"
   (mis-recipes)
   (lambda (x)
     (if (string-match "^\\([^-]+\\)-\\(.*\\)$" x)
         (find-file
          (mis-build-path
           (mis-directory)
           (match-string 1 x)
           (match-string 2 x)
           "Makefile"))
       (error "Failed to split %s" x)))))

(defun mis-create-makefile (action)
  "Create a new Makefile for ACTION."
  (let* ((olde (file-name-extension (car mis-current-files)))
         (newe (if (string-match "^to-\\(.*\\)" action)
                   (match-string 1 action)
                 (concat "out." olde)))
         (preamble (concat (substring mis-makefile-preamble 1)
                           "\n\n#" (make-string 78 ?_) "\n"))
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
         (t-phony ".PHONY: all install-tools clean"))
    (mapconcat 'identity
               (list preamble olds news t-all t-new
                     t-clean t-tools t-phony)
               "\n\n")))

;;;###autoload
(defun make-it-so (arg)
  "When called from `dired', offer a list of transformations.
Available trasformations are dispatched on currently selected
file(s)' extension.  Therefore it's an error when files with
multiple extensions are marked.  After an action is selected,
proceed to call `mis-action' for that action."
  (interactive "p")
  (cl-case major-mode
    (wdired-mode
     (call-interactively 'self-insert-command))
    (dired-mode
     (let ((dispatch-file (car (dired-get-marked-files nil t)))
           (files (dired-get-marked-files)))
       (setq mis-current-files
             (if (member dispatch-file files)
                 (cons dispatch-file
                       (delete dispatch-file files))
               files))
       (if (or (mis-all-equal
                (mapcar #'file-name-extension mis-current-files))
               (= arg 2)
               (y-or-n-p "Mixed extensions in selection, continue?"))
           (let* ((ext (file-name-extension (car mis-current-files)))
                  (candidates (if (= arg 2)
                                  (mis-recipes-by-ext "mix")
                                (mis-recipes-by-ext ext))))
             (mis-competing-read
              "Makefile: " candidates 'mis-action))
         (error "Mixed extensions in selection"))))
    (t
     (error "Must be called from dired"))))

;;;###autoload
(defun mis-abort ()
  "Abort tranformation.
This function should revert to the state before `mis-action' was called."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (let* ((makefile-buffer (find-buffer-visiting (expand-file-name "Makefile")))
         (dir (expand-file-name default-directory))
         (dired-buffer
          (cl-find-if
           (lambda (b)
             (with-current-buffer b
               (and (eq major-mode 'dired-mode)
                    (equal (expand-file-name default-directory) dir))))
           (buffer-list)))
         (targets (read (mis-slurp "targets")))
         (first-target (car targets))
         (sources (read (mis-slurp "sources"))))
    (when makefile-buffer
      (kill-buffer makefile-buffer))
    (when dired-buffer
      (kill-buffer dired-buffer))
    (let (from to)
      (while (setq from (pop targets))
        (setq to (pop sources))
        (make-directory (file-name-directory to) t)
        (rename-file from to)))
    (let ((inhibit-message t))
      (dired (file-name-directory (directory-file-name dir))))
    (delete-directory dir t)
    (revert-buffer)
    (let ((pt (point)))
      (goto-char (point-min))
      (when (string-match "\\.mix\\'" first-target)
        (setq first-target (substring first-target 0 (match-beginning 0))))
      (if (search-forward (file-name-nondirectory first-target))
          (goto-char (match-beginning 0))
        (goto-char pt)))))

;;;###autoload
(defun mis-finalize ()
  "Finalize transformation.
In addition to `mis-abort' copy over the files listed in
\"provide\".  Each Makefile should append all essential files
that it creates to a \"provide\" file.  All generated files not in \"provide\",
i.e. intermediates and logs and such, will be deleted."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (unless (file-exists-p "provide")
    (error "No provide in current directory"))
  (let ((provides (split-string (mis-slurp "provide") "\n" t))
        (compilation (get-buffer "*compilation*")))
    (when provides
      (mapc (lambda (f) (mis-rename-unquote f (expand-file-name ".."))) provides)
      (when compilation
        (kill-buffer compilation)))
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
  "Choose \"mis-\" via completion."
  (interactive)
  (mis-competing-read
   "Action: "
   (mapcar
    (lambda (x)
      (list
       (format "% -30s%s" x
               (or (cdr (assoc x mis-bindings-alist))
                   "not bound"))
       x))
    '(mis-finalize mis-abort mis-replace))
   (lambda (x) (call-interactively (car x)))))

;;;###autoload
(defun mis-save-and-compile ()
  "Save current buffer and call `compile' with `mis-make-command'.
Switch to other window afterwards."
  (interactive)
  (save-buffer)
  (compile mis-make-command))

;;* Utilities
(defun mis-directory-files (directory)
  "Return results of (`directory-files' DIRECTORY) without \".\" and \"..\"."
  (and (file-exists-p directory)
       (delete "." (delete ".." (directory-files directory)))))

(defun mis-recipes-by-ext (ext)
  "Return a list of recipes available for EXT."
  (setq ext (or ext "nil"))
  (mis-directory-files
   (expand-file-name ext (mis-directory))))

(defun mis-recipes ()
  "Return a list of current recipes."
  (let ((formats (mis-directory-files (mis-directory))))
    (apply #'append
           (cl-loop for f in formats
                    collect
                    (mapcar (lambda (x) (format "%s-%s" f x))
                            (mis-recipes-by-ext f))))))

(defun mis-build-path (&rest lst)
  "Build a path from LST."
  (cl-reduce (lambda (a b) (expand-file-name b a)) lst))

(defun mis-build-path-create (&rest lst)
  "Build a path from LST.  Create intermediate directories."
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
  (let* ((mix-p (eq current-prefix-arg 2))
         (sources mis-current-files)
         (source (file-name-nondirectory (car sources)))
         (ext (if mix-p "mix" (file-name-extension source)))
         (basedir (or (file-name-directory source)
                      default-directory))
         (dir (expand-file-name
               (format "%s_%s" x (file-name-nondirectory source))
               basedir))
         (makefile-template
          (mis-build-path (mis-directory) ext x "Makefile"))
         (makefile-name (expand-file-name "Makefile" dir)))
    (mkdir dir)
    (let ((targets (mapcar
                    (lambda (x)
                      (mis-rename-quote x dir mix-p))
                    sources)))
      ;; If a recipe exists, copy it.
      ;; Otherwise create a new one, move it here and mark it to be
      ;; restored to the proper location.
      (if (file-exists-p makefile-template)
          (with-temp-buffer
            (insert (substring mis-makefile-key-descriptions 1) "\n\n")
            (insert-file-contents makefile-template)
            (write-file makefile-name))
        (let ((package-location (expand-file-name ".." mis-recipes-directory)))
          (unless (file-exists-p (expand-file-name ".git" package-location))
            (warn "%s is not version controlled, recipes may be lost on package update"
                  package-location)))
        (mis-spit (mis-create-makefile x) makefile-name)
        (push makefile-name targets)
        (push makefile-template sources))
      (mis-spit (prin1-to-string targets)
                (expand-file-name "targets" dir))
      (mis-spit (prin1-to-string sources)
                (expand-file-name "sources" dir))
      (find-file makefile-name)
      (mis-makefile-mode))))

(defun mis-delete-file (file)
  "Delete FILE."
  (move-file-to-trash file))

(defun mis-rename-quote (file dir &optional mix)
  "Move FILE to DIR, changing spaces to underscores."
  (let ((dest (expand-file-name
               (replace-regexp-in-string
                ":" "β"
                (replace-regexp-in-string
                 " " "α" (file-name-nondirectory file))) dir)))
    (when mix
      (setq dest (concat dest ".mix")))
    (rename-file file dest)
    dest))

(defun mis-rename-unquote (file dir)
  "Move FILE to DIR, changing spaces to underscores."
  (let ((dest (expand-file-name
               (replace-regexp-in-string
                "β" ":"
                (replace-regexp-in-string
                 "α" " " (file-name-nondirectory file)))
               dir)))
    (rename-file file dest)
    dest))

(defun mis-directory ()
  "A getter for `mis-recipes-directory'."
  (if (file-exists-p mis-recipes-directory)
      mis-recipes-directory
    ;; look for recipes in package directory
    (let* ((default-directory package-user-dir)
           (dirs (file-expand-wildcards "*make-it-so*")))
      (if (= 1 (length dirs))
          (setq mis-recipes-directory
                (mis-build-path (car dirs) "recipes"))
        (error "Not one make-it-so in package dir")))))

(provide 'make-it-so)

;;; make-it-so.el ends here
