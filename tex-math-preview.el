;;; tex-math-preview.el --- preview TeX math expressions.

;; Copyright 2006, 2007, 2008 Kevin Ryde

;; Author: Kevin Ryde <user42@zip.com.au>
;; Version: 5
;; Keywords: tex
;; URL: http://www.geocities.com/user42_kevin/tex-math-preview/index.html
;; EmacsWiki: TexMathPreview
;; Compatibility: Emacs 21, Emacs 22
;; Incompatibility: XEmacs 21

;; tex-math-preview.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; tex-math-preview.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.

;;; Commentary:

;; M-x tex-math-preview previews TeX math expressions.  Put point in a TeX,
;; LaTeX, Texinfo, Wikipedia or DBTexMath math expression and M-x
;; tex-math-preview shows either an image or TeX error messages.
;;
;; `tex-mode' has its own far more substantial buffer and region previewing,
;; but tex-math-preview is intentionally simpler and is geared towards
;; unsophisticated TeX users.

;;; Emacsen:

;; Designed for Emacs 21 and 22.
;;
;; Doesn't work in XEmacs due to some missing functions there and because
;; `shell-command' doesn't return an exit status (could add the extra
;; functions but redoing shell-command is too much like hard work).

;;; Install:

;; To make M-x tex-math-preview available put tex-math-preview.el somewhere
;; in your load-path and the following in your .emacs
;;
;;     (autoload 'tex-math-preview "tex-math-preview" nil t)
;;
;; Bind it to a key if you like, eg. f8 in texinfo-mode,
;;
;;     (add-hook 'texinfo-mode-hook
;;                (lambda ()
;;                  (define-key texinfo-mode-map [f8] 'tex-math-preview)))

;;; History:

;; Version 1 - the first version
;; Version 2 - multi-line <math> reported by Uwe Brauer; better $...$ matching
;; Version 3 - also images in a buffer using dvipng
;; Version 4 - yet better $...$ handling, add some dbtexmath
;; Version 5 - add latex \(...\) and \[...\]

;;; Code:

(require 'thingatpt)
(require 'tex-mode)   ;; for tex-dvi-view-command


;;;###autoload
(defgroup tex-math-preview nil
  "Tex Math Preview."
 :prefix "tex-math-preview-"
 :group 'applications
 :link  '(url-link
          :tag "tex-math-preview home page"
          "http://www.geocities.com/user42_kevin/tex-math-preview/index.html"))

(defcustom tex-math-preview-function
  'tex-math-preview-adaptview
  "Function for `tex-math-preview' to show a DVI file.
The default `tex-math-preview-adaptview' chooses among the
methods, according to what Emacs and the system supports."
  :type '(choice (tex-math-preview-adaptview
                  tex-math-preview-dvi-view
                  tex-math-preview-png-image)
                 function)
  :group 'tex-math-preview)


;;-----------------------------------------------------------------------------

(defun tex-math-preview-bounds-of-tex-math ()
  "A `bounds-of-thing-at-point' function for a TeX maths expression.
See `tex-math-preview' for what's matched.
The return is a pair of buffer positions (START . END), or nil if
no recognised expression at or surrounding point."

  ;; TeX style $...$ could easily match some huge chunk of the buffer, and
  ;; even @math{...} or <math>...</math> could occur in comments or some
  ;; unrelated context.  So it's not reliable just to take the first of
  ;; these which match, instead the strategy is to check for all forms
  ;; around point and take the one that's the smallest.
  ;;
  ;; Only the start position of the match is considered for "smallest", the
  ;; one that's the shortest distance before point (but covering point of
  ;; course) in the buffer is taken.

  (let (case-fold-search beg end)

    ;; $...$ and $$...$$
    ;; thing-at-point-looking-at doesn't work on "$...$".  The way the start
    ;; and end are the same (ie. "$") breaks the straightforward
    ;; implementation of that function; so the idea here is to search back
    ;; for the starting "$", and one not "\$" escaped, then check the $...$
    ;; extent covers point
    (save-excursion
      (while (and (search-backward "$" nil t) ;; $ not preceded by \
                  (eq ?\\ (char-before))))
      (when (looking-at "\\$+\\(\\(?:\\\\\\$\\|[^$]\\)+?\\)\\$")
        (setq beg (match-beginning 1) end (match-end 1))))

    (dolist (elem
             '(;; <math>...</math>
               (1 . "<math>\\(\\(.\\|\n\\)*?\\)</math>")

               ;; @math{...}
               (1 . "@math{\\(\\(.\\|\n\\)*?\\)}")

               ;; <alt role="tex">$...$</alt>
               ;; <alt role="tex">\[...\]</alt>
               ;; the contents $..$ or \[..\] of the alt can be recognised
               ;; on their own, but with this pattern we can work with point
               ;; in the <alt> part as well as in the expression
               (1 . "<alt\\s-+role=\"tex\">\\$*\\(\\(.\\|\n\\)+?\\)\\$*</alt>")

               ;; \[...\]
               (0 . "\\\\\\[\\(.\\|\n\\)*?\\\\]")

               ;; \(...\)
               (0 . "\\\\(\\(.\\|\n\\)*?\\\\)")

               ;; \begin{math}...\end{math}
               (0 . "\\\\begin{math}\\(\\(.\\|\n\\)*?\\)\\\\end{math}")

               ;; \begin{displaymath}...\end{displaymath}
               (0 . "\\\\begin{displaymath}\\(\\(.\\|\n\\)*?\\)\\\\end{displaymath}")))

      (when (thing-at-point-looking-at (cdr elem))
        ;; if no other match, or this match is later, then override
        (if (or (not beg)
                (> (match-beginning (car elem)) beg))
            (setq beg (match-beginning (car elem)) end (match-end (car elem))))))

    (and beg
         (cons beg end))))
          
(put 'tex-math 'bounds-of-thing-at-point 'tex-math-preview-bounds-of-tex-math)

;;;###autoload
(defun tex-math-preview ()
  "Preview a TeX maths expression at (or surrounding) point.
The `tex-math-preview-function' variable controls the viewing
method.  The math expressions recognised are

    $...$ or $$...$$              TeX
    \\(...\\) or \\=\\[...\\]            LaTeX
    \\begin{math}...\\end{math}     LaTeX
    \\begin{displaymath}...        LaTeX
    @math{...}                    Texinfo
    <math>...</math>              Wikipedia
    <alt role=\"tex\">...</alt>     DBTexMath

DBTexMath is processed with plain TeX by default, or if it
contains \\(...\\) or \\=\\[...\\] then with LaTeX.

\"$\" is both the start and end for plain TeX, making it slightly
ambiguous.  tex-math-preview assumes point is inside the
expression, so when just after a \"$\" then that's the start, or
when just before then that's the end.  If point is in between two
\"$$\" then that's considered a start.

For more on the respective formats see

    URL `http://www.latex-project.org/'
    Info node `(texinfo)math'
    URL `http://meta.wikimedia.org/wiki/Help:Displaying_a_formula'
    URL `http://ricardo.ecn.wfu.edu/~cottrell/dbtexmath/'"

  (interactive)
  (let ((str (thing-at-point 'tex-math)))
    (or str
        (error "Not in a TeX math expression"))
    (tex-math-preview-str str)))

(defun tex-math-preview-str (str)
  "Preview the given STR string as a TeX math expression.
STR should not have $ or $$ delimiters."

  (let* ((tex-math-dir (make-temp-file "tex-math-preview-" t))
         (dot-tex      (concat tex-math-dir "/foo.tex"))
         (dot-dvi      (concat tex-math-dir "/foo.dvi"))
         (dot-log      (concat tex-math-dir "/foo.log"))
         (dot-aux      (concat tex-math-dir "/foo.aux"))
         ;; \[ or \( or \begin anywhere means latex
         (tex-cmd      (if (string-match "\\\\\\([[(]\\|begin\\)" str)
                           "latex" "tex")))

    (with-temp-file dot-tex
      (if (equal "tex" tex-cmd)
          (progn
            ;; must be careful with the newlines here, a blank line inside
            ;; $$...$$ would be a paragraph separator, which tex doesn't
            ;; allow; let any in the user's input go through to see the
            ;; error, but be careful not to add a \n here before or after $$
            ;; in case that and the user input makes \n\n
            (insert "$$ ")
            (insert str)
            (insert " $$\n\\par\\bye\n"))

        (insert "\\documentclass{article}\n")
        (insert "\\pagestyle{empty}\n")
        (insert "\\begin{document}\n")
        (insert str)
        (insert "\\par\n")
        (insert "\\end{document}\n")))

    (unwind-protect
        ;; don't show all the tex ramblings in the minibuffer, leave it to
        ;; the shell buffer, and show that only if there's an error (ie. put
        ;; back window config if no error)
        ;;
        (let ((max-mini-window-height 1)  ;; force shell-command to buffer
              (windows (current-window-configuration)))
          (if (not (eq 0 (shell-command
                          (concat tex-cmd " -output-directory " tex-math-dir
                                  " " dot-tex " </dev/null"))))
              (error "TeX processing error")

            (set-window-configuration windows)
            (funcall tex-math-preview-function dot-dvi)))

      ;; cleanup temp files
      (dolist (filename (list dot-tex dot-dvi dot-log dot-aux))
        (condition-case nil (delete-file filename) (error)))
      (delete-directory tex-math-dir))))


;;-----------------------------------------------------------------------------
;; adaptive viewer selection

(defun tex-math-preview-adaptview (filename)
  "Display dvi FILENAME using either png image or `tex-dvi-view-command'.
A PNG image in a buffer per `tex-math-preview-png-image' is used
if possible, or if not then the `tex-mode' previewer given by
`tex-dvi-view-command' (like `tex-math-preview-dvi-view' uses).

This function is the default for `tex-math-preview-function',
allowing `tex-math-preview' to adapt to the Emacs display
capabilities and available viewer program(s)."

  (if (and (image-type-available-p 'png)
           (display-images-p)
           (eq 0 (shell-command "dvipng --version >/dev/null 2>&1" nil)))
      (tex-math-preview-png-image filename)
      (tex-math-preview-dvi-view filename)))


;;-----------------------------------------------------------------------------
;; view by running tex-dvi-view-command

(defun tex-math-preview-dvi-view (filename)
  "Display dvi FILENAME using `tex-dvi-view-command'.
This can be used in `tex-math-preview-function'.

The default `tex-dvi-view-command' under X is xdvi and it works
well.  On an SVGA console of a GNU/Linux system you can use
dvisvga (from tmview), or perhaps try a combination of dvipng (or
dvips+ghostscript) and a console image viewer like zgv.  Any
program output is shown in a buffer, which is good for error
messages but if it prints a startup banner etc you'll want to
find a \"quiet\" mode or use a wrapper script to grep that out."

  ;; eval/expand like `tex-view' and `tex-send-command' do
  (let* ((template (eval tex-dvi-view-command))
         (command  (replace-regexp-in-string "\\*" filename
                                             template t t)))
    (if (string-equal command template)
        (setq command (concat command " " filename)))
    (shell-command command)))


;;-----------------------------------------------------------------------------
;; view png in a buffer

(defun tex-math-preview-png-image (filename)
  "Display dvi FILENAME as a png image in a buffer.
This can be used in `tex-math-preview-function', but it requires:

* the \"dvipng\" program (http://sourceforge.net/projects/dvipng/)
* a display which can show images (eg. X, not a tty)
* Emacs built with the PNG image libraries"

  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (let ((image (tex-math-preview-dvi-to-image filename)))
    (if image
        (save-selected-window
          (switch-to-buffer-other-window "*tex-math-preview*")
          (erase-buffer)
          (insert "\n")
          (insert-image image " ")
          (goto-char (point-min))))))

(defun tex-math-preview-dvi-to-image (filename)
  "Render dvi FILENAME to an Emacs image and return that.
The \"dvipng\" program is used for drawing.  If it fails a shell
buffer is left showing the messages and the return is nil."

  (let ((dot-png (concat tex-math-dir "/foo.png")))
    (when (eq 0 (shell-command (concat "dvipng -x 1728 -o" dot-png
                                       " " filename)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally dot-png)
        ;; use :data for the image so we can delete the file
        (prog1 `(image :type png :data ,(buffer-string))
          (delete-file dot-png))))))

(provide 'tex-math-preview)

;;; tex-math-preview.el ends here
