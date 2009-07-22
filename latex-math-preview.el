;; latex-math-preview.el --- preview LaTeX mathematical expressions.

;; latex-math-preview.el is derived from tex-math-preview.el.
;; This emacs lisp is made by reducing some features of tex-math-preview.el
;; and adjusting it to files of which format is only LaTeX.
;; tex-math-preview.el is made by Kevin Ryde and 
;; has some features which latex-math-preview does not have.
;; Please see http://user42.tuxfamily.org/tex-math-preview/index.html
;; for details of tex-math-preview.el.

;; latex-math-preview.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; latex-math-preview.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
;; Public License for more details.
;;
;; You can get a copy of the GNU General Public License online at
;; <http://www.gnu.org/licenses>.

;; Commentary:
;; M-x latex-math-preview previews mathematical expressions pointed
;; by cursor in LaTeX files.
;; The result of latex-math-preview is shown in new buffer as image.

;; Requirements;
;; Emacs 22 or 23.
;; dvipng
;; latex

;; Install:
;; Put latex-math-preview.el to your load-path and
;; write the following code in ~/.emacs.el.
;; 
;;   (autoload 'latex-math-preview "latex-math-preview" nil t)
;; 
;; For YaTeX mode, add the follwing to ~/.emacs.el if desired.
;;
;;   (add-hook 'yatex-mode-hook
;; 	      (YaTeX-define-key "p" 'latex-math-preview))

;; Settings
;; You can customize some variables.
;; In particular, please set the value of the following variables if needed.
;;  latex-math-preview-latex-command
;;  latex-math-preview-command-dvipng
;;  latex-math-preview-latex-template-header
;;  latex-math-preview-latex-template-footer
;; 
;; latex-math-preview makes a temporary latex file and compiles it and 
;; so gets a preview image.
;; latex-math-preview-latex-command is the path to command
;; 'latex', 'platex' or etc.
;; latex-math-preview-command-dvipng is the path to command 'dvipng'.
;; The construction of temporary latex file is the following.
;; 
;; (part of latex-math-preview-latex-template-header
;;  the default value is the following)
;; \documentclass{article}
;; \usepackage{amsmath, amsfonts, amsthm}
;; \pagestyle{empty}
;; \begin{document}
;;
;; (some mathematical expressions)
;;
;; (part of latex-math-preview-latex-template-footer
;;  the default value is the following)
;; \par
;; \end{document}
;; 
;; So, if you can use some latex packages in temporary latex files,
;; you should set the customized value to
;; latex-math-preview-latex-template-header.

;; History:
;; 2009/07/22 version 0.0.1 release

;;; Code:

(require 'thingatpt)
(require 'tex-mode)   ;; for tex-dvi-view-command


;;;###autoload
(defgroup latex-math-preview nil
  "Tex Math Preview."
 :prefix "latex-math-preview-"
 :group 'applications
 :link  '(url-link
          :tag "latex-math-preview home page"
          "http://www.geocities.com/user42_kevin/latex-math-preview/index.html"))

(defcustom latex-math-preview-function
  'latex-math-preview-adaptview
  "Function for `latex-math-preview' to show a DVI file.
The default `latex-math-preview-adaptview' chooses among the
methods, according to what Emacs and the system supports."
  :type '(choice (latex-math-preview-adaptview
                  latex-math-preview-dvi-view
                  latex-math-preview-png-image)
                 function)
  :group 'latex-math-preview)

(defvar latex-math-preview-buffer-name
  "*latex-math-preview*"
  "Name of buffer which displays preview image.")

(defvar latex-math-preview-latex-command
  "latex"
  "Path to latex.")

(defvar latex-math-preview-command-dvipng
  "dvipng"
  "Path to dvipng.")

(defvar latex-math-preview-temporary-file-prefix
  "temp_latex_math"
  "The prefix name of some temporary files which is produced in making an image.")

(defvar latex-math-preview-dvipng-option
  "-x 1728 -T tight"
  "Option for dvipng.")

(defvar latex-math-preview-latex-template-header
  "\\documentclass{article}\n\\usepackage{amsmath, amsfonts, amsthm}\n\\pagestyle{empty}\n\\begin{document}\n"
  "Insert string to beggining of temporary latex file to make image.")

(defvar latex-math-preview-latex-template-footer
  "\\par\n\\end{document}\n"
  "Insert string to end of temporary latex file to make image.")

(defvar latex-math-preview-match-expression
  '(
    ;; \[...\]
    (0 . "\\\\\\[\\(.\\|\n\\)*?\\\\]")

    ;; \(...\)
    (0 . "\\\\(\\(.\\|\n\\)*?\\\\)")

    ;; \begin{math}...\end{math}
    (0 . "\\\\begin{math}\\(\\(.\\|\n\\)*?\\)\\\\end{math}")

    ;; \begin{displaymath}...\end{displaymath}
    (0 . "\\\\begin{displaymath}\\(\\(.\\|\n\\)*?\\)\\\\end{displaymath}")

    ;; \begin{equation}...\end{equation}
    (0 . "\\\\begin{equation\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{equation\\(\\|\\*\\)}")

    ;; \begin{gather}...\end{gather}
    (0 . "\\\\begin{gather\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{gather\\(\\|\\*\\)}")

    ;; \begin{align}...\end{align}
    (0 . "\\\\begin{align\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{align\\(\\|\\*\\)}")

    ;; \begin{alignat}...\end{alignat}
    (0 . "\\\\begin{alignat\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{alignat\\(\\|\\*\\)}")

    ;; \begin{multiline}...\end{multiline}
    (0 . "\\\\begin{multiline\\(\\|\\*\\)}\\(\\(.\\|\n\\)*?\\)\\\\end{multiline\\(\\|\\*\\)}")
    )
  "These eqpressions are used for matching to extract tex math expression.")

(defvar latex-math-preview-map
  (let ((map (copy-keymap view-mode-map)))
    (define-key map (kbd "q") 'delete-window)
    map)
  "Keymap for latex-math-preview.")

;;-----------------------------------------------------------------------------

(defun latex-math-preview-bounds-of-latex-math ()
  "A `bounds-of-thing-at-point' function for a TeX maths expression.
See `latex-math-preview' for what's matched.
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
      (when (looking-at "\\(\\$+\\(?:\\\\\\$\\|[^$]\\)+?\\$\\)")
        (setq beg (match-beginning 1) end (match-end 1))))

    (dolist (elem latex-math-preview-match-expression)
      (when (thing-at-point-looking-at (cdr elem))
        ;; if no other match, or this match is later, then override
        (if (or (not beg)
                (> (match-beginning (car elem)) beg))
            (setq beg (match-beginning (car elem)) end (match-end (car elem))))))

    (and beg
         (cons beg end))))
          
(put 'latex-math 'bounds-of-thing-at-point 'latex-math-preview-bounds-of-latex-math)

;;;###autoload
(defun latex-math-preview ()
  "Preview a TeX maths expression at (or surrounding) point.
The `latex-math-preview-function' variable controls the viewing
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
ambiguous.  latex-math-preview assumes point is inside the
expression, so when just after a \"$\" then that's the start, or
when just before then that's the end.  If point is in between two
\"$$\" then that's considered a start.

For more on the respective formats see

    URL `http://www.latex-project.org/'
    Info node `(texinfo)math'
    URL `http://meta.wikimedia.org/wiki/Help:Displaying_a_formula'
    URL `http://ricardo.ecn.wfu.edu/~cottrell/dbtexmath/'"

  (interactive)
  (let ((str (thing-at-point 'latex-math)))
    (or str
        (error "Not in a TeX math expression"))
    (latex-math-preview-str str)))

(defun latex-math-preview-str (str)
  "Preview the given STR string as a TeX math expression.
STR should not have $ or $$ delimiters."

  (let* ((latex-math-dir (make-temp-file "latex-math-preview-" t))
         (dot-tex      (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".tex"))
         (dot-dvi      (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".dvi"))
         (dot-log      (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".log"))
         (dot-aux      (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".aux")))

    (with-temp-file dot-tex
	(insert latex-math-preview-latex-template-header)
        (insert str)
	(insert latex-math-preview-latex-template-footer)
        )

    (unwind-protect
        ;; don't show all the tex ramblings in the minibuffer, leave it to
        ;; the shell buffer, and show that only if there's an error (ie. put
        ;; back window config if no error)
        ;;
        (let ((max-mini-window-height 1)  ;; force shell-command to buffer
              (windows (current-window-configuration)))
          (if (not (eq 0 (shell-command
                          (concat latex-math-preview-latex-command " -output-directory " latex-math-dir
                                  " " dot-tex " </dev/null"))))
              (error "TeX processing error")

            (set-window-configuration windows)
            (funcall latex-math-preview-function dot-dvi)))

      ;; cleanup temp files
      (dolist (filename (list dot-tex dot-dvi dot-log dot-aux))
        (condition-case nil (delete-file filename) (error)))
      (delete-directory latex-math-dir)
      )))


;;-----------------------------------------------------------------------------
;; adaptive viewer selection

(defun latex-math-preview-adaptview (filename)
  "Display dvi FILENAME using either png image or `tex-dvi-view-command'.
A PNG image in a buffer per `latex-math-preview-png-image' is used
if possible, or if not then the `tex-mode' previewer given by
`tex-dvi-view-command' (like `latex-math-preview-dvi-view' uses).

This function is the default for `latex-math-preview-function',
allowing `latex-math-preview' to adapt to the Emacs display
capabilities and available viewer program(s)."

  (if (and (image-type-available-p 'png)
           (display-images-p)
           (eq 0 (shell-command (concat latex-math-preview-command-dvipng " --version >/dev/null 2>&1") nil)))
      (latex-math-preview-png-image filename)
      (latex-math-preview-dvi-view filename)))


;;-----------------------------------------------------------------------------
;; view by running tex-dvi-view-command

(defun latex-math-preview-dvi-view (filename)
  "Display dvi FILENAME using `tex-dvi-view-command'.
This can be used in `latex-math-preview-function'.

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

(defun latex-math-preview-png-image (filename)
  "Display dvi FILENAME as a png image in a buffer.
This can be used in `latex-math-preview-function', but it requires:

* the \"dvipng\" program (http://sourceforge.net/projects/dvipng/)
* a display which can show images (eg. X, not a tty)
* Emacs built with the PNG image libraries"

  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (let ((image (latex-math-preview-dvi-to-image filename)))
    (if image
	(progn
          (switch-to-buffer-other-window latex-math-preview-buffer-name)
	  (setq view-read-only nil)
          (erase-buffer)
          (insert "\n")
          (insert-image image " ")
          (goto-char (point-min))
	  (setq view-read-only t)
	  (use-local-map latex-math-preview-map)
	  ))))

(defun latex-math-preview-dvi-to-image (filename)
  "Render dvi FILENAME to an Emacs image and return that.
The \"dvipng\" program is used for drawing.  If it fails a shell
buffer is left showing the messages and the return is nil."

  (let ((dot-png (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".png")))
    (when (eq 0 (shell-command
		 (concat latex-math-preview-command-dvipng " " latex-math-preview-dvipng-option
			 " -o" dot-png " " filename)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally dot-png)
        ;; use :data for the image so we can delete the file
        (prog1 `(image :type png :data ,(buffer-string))
          (delete-file dot-png))))))

(provide 'latex-math-preview)

;; latex-math-preview.el ends here
