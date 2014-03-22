;;; make-it-so.el --- Transform file with Makefile recipes. -*- lexical-binding: t -*-

;; Copyright (C) 2014 Oleh Krehel

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/make-it-so
;; Version: 0.1.0
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
;; to `mis-make-it-so'.  You'll be prompted to select a transformation
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

;; ——— Customization ———————————————————————————————————————————————————————————
(defgroup make-it-so nil
  "Transfrom files in `dired' with Makefile recipes."
  :group 'dired
  :prefix "mis-")

(defcustom mis-recipes-directory "~/git/make-it-so/recipes/"
  "Directory with available recipes."
  :type 'directory
  :group 'make-it-so)

(defvar mis-mode-map
  (make-sparse-keymap))

(defvar mis-source-files nil
  "Current set of files to transform.")

(defvar mis-target-files nil
  "Mapping of `mis-source-files' to the transformation directory.
So to cancel the transformation, rename each file in
`mis-target-files' to `mis-source-files'.")

;; ——— Interactive —————————————————————————————————————————————————————————————
;;;###autoload
(define-minor-mode mis-mode
    "Add make-it-so key bindings to `dired'.

\\{mis-mode-map}"
  :keymap mis-mode-map
  :group 'make-it-so)

(let ((map mis-mode-map))
  (define-key map (kbd ",") 'mis-make-it-so)
  (define-key map (kbd "C-,") 'mis-finalize)
  (define-key map (kbd "C-M-,") 'mis-abort))

;;;###autoload
(defun mis-mode-on ()
  "Enable make-it-so bindings."
  (mis-mode 1))

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

;;;###autoload
(defun mis-make-it-so ()
  "When called from `dired', offer a list of transformations."
  (interactive)
  (setq mis-source-files (dired-get-marked-files nil current-prefix-arg))
  (if (mis-all-equal (mapcar #'file-name-extension mis-source-files))
      (let* ((ext (file-name-extension (car mis-source-files)))
             (candidates (mis-recipes-by-ext ext)))
        (if candidates
            (helm :sources
                  `((name . "Tools")
                    (candidates . ,candidates)
                    (action . mis-action)))
          (error "No candidates for *.%s" ext)))
    (error "Mixed extensions in selection")))

;;;###autoload
(defun mis-abort ()
  "Abort tranformation.
This function should revert to the state before `mis-make-it-so' was called."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (let ((makefile-buffer (find-buffer-visiting (expand-file-name "Makefile"))))
    (when makefile-buffer
      (kill-buffer makefile-buffer)))
  (cl-mapcar 'rename-file mis-target-files mis-source-files)
  (let ((dir default-directory))
    (dired "..")
    (delete-directory dir t))
  (revert-buffer))

;;;###autoload
(defun mis-finalize ()
  "Finalize transformation.
In addition to `mis-abort' copy over the files listed in \"provide\"."
  (interactive)
  (unless (file-exists-p "Makefile")
    (error "No Makefile in current directory"))
  (unless (file-exists-p "provide")
    (error "No provide in current directory"))
  (let ((provides (mis-slurp "provide")))
    (setq provides (split-string provides "\n" t))
    (mapc (lambda (f) (mis-rename-unquote f (expand-file-name ".."))) provides)
    (mis-abort)))

;;;###autoload
(defun mis-replace ()
  "Finalize transformation and move source files to trash."
  (interactive)
  (mis-finalize)
  (mapc 'mis-delete-file mis-source-files)
  (revert-buffer))

;; ——— Utilities ———————————————————————————————————————————————————————————————
(defun mis-directory-files (directory)
  "Return results of (`directory-files' DIRECTORY) without \".\" and \"..\"."
  (delete "." (delete ".." (directory-files directory))))

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

(defun mis-slurp (file)
  "Return contents of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

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
  (let* ((source (file-name-nondirectory (car mis-source-files)))
         (ext (file-name-extension source))
         (basedir (or (file-name-directory source)
                      default-directory))
         (dir (expand-file-name
               (format "%s:%s" x (file-name-nondirectory source))
               basedir))
         (makefile-template
          (mis-build-path mis-recipes-directory ext x "Makefile"))
         (makefile (expand-file-name "Makefile" basedir)))
    (unless (file-exists-p makefile-template)
      (error "File not found %s" makefile-template))
    (copy-file makefile-template makefile)
    (let ((requires (shell-command-to-string "make require")))
      (if (string-match "make:" requires)
          (error "Makefile must have a \"require\" target")
        (mkdir dir)
        (setq mis-source-files
              (append mis-source-files
                      (mapcar 'expand-file-name
                              (split-string requires "\n" t))))
        (setq mis-target-files
              (mapcar (lambda (x) (mis-rename-quote x dir))
                      mis-source-files))
        (rename-file "Makefile" dir)
        ;; (mis-spit requires (expand-file-name "requires" dir))
        ))
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
