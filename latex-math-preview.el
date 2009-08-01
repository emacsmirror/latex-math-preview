;;; latex-math-preview.el --- preview LaTeX mathematical expressions.

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: LaTeX TeX
;; Version: 0.1.1
;; Created: Fri Jul 31 16:29:50 2009

;; latex-math-preview.el is a modified version which is based on
;; tex-math-preview.el and has been created at July 2009.
;; This emacs lisp is made by reducing some features of tex-math-preview.el
;; and adjusting it to LaTeX files.
;; tex-math-preview.el is made by Kevin Ryde and 
;; has some features which latex-math-preview does not have.
;; Please see http://user42.tuxfamily.org/tex-math-preview/index.html
;; for details of tex-math-preview.el.

;; Copyright 2006, 2007, 2008, 2009 Kevin Ryde
;; Copyright 2009 Takayuki YAMAGUCHI
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later 
;; version. 
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT 
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; M-x latex-math-preview previews mathematical expressions pointed
;; by cursor in LaTeX files.
;; The result of latex-math-preview is shown in new buffer as image
;; or as dvi file by dvi viewer.

;; Requirements;
;; Emacs (version 22 or 23) on Linux or Meadow3 on Windows.
;; dvipng
;; latex or platex
;; dvi viewer (for example, xdvi) if needed

;;; Install:
;; Put latex-math-preview.el to your load-path and
;; write the following code in ~/.emacs.el.
;; 
;;   (autoload 'latex-math-preview "latex-math-preview" nil t)
;; 
;; For YaTeX mode, add the follwing to ~/.emacs.el if desired.
;;
;;   (add-hook 'yatex-mode-hook
;;            '(lambda ()
;; 	      (YaTeX-define-key "p" 'latex-math-preview)))
;;
;; This setting almost binds latex-math-preview to "C-c p".

;;; Settings:
;; You can customize some variables.
;; In particular, please set the value of the following variables
;; according to your system if needed.
;;  latex-math-preview-latex-command
;;  latex-math-preview-command-dvipng
;;  latex-math-preview-command-dvi-view
;;  latex-math-preview-latex-template-header
;;  latex-math-preview-latex-template-footer
;; 
;; latex-math-preview makes a temporary latex file and compiles it and 
;; then gets a preview image by dvipng.
;; latex-math-preview-latex-command is the path to command
;; 'latex', 'platex' or etc.
;; latex-math-preview-command-dvipng is the path to command 'dvipng'.
;; The example of setting values for unix or linux is the following.
;;  (setq latex-math-preview-latex-command "/usr/bin/platex")
;;  (setq latex-math-preview-command-dvipng "/usr/bin/dvipng")
;;  (setq latex-math-preview-command-dvi-view "/usr/bin/xdvi")
;;
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
;; latex-math-preview compiles a file like the above and
;; the produced dvi file is converted to png by dvipng.
;; Then this png file is shown in the buffer.
;; 
;; If you want to set the options for dvipng, you may set some variables.
;; latex-math-preview-dvipng-option is used as options of dvipng.
;; latex-math-preview-image-foreground-color and
;; latex-math-preview-image-background-color define the foreground and
;; background colors of png images respectively.
;; If these variables are nil, these colors are the same as it of default face.

;;; Usage:
;; If you type "M-x tex-math-preview" when cursor points to 
;; a mathematical expression, new buffer including an image
;; is created when you have configuration to use png file.
;; In this buffer, you check the result of LaTeX mathematical
;; expression and type 'q' to exit the window.
;; 
;; The default setting supports the following LaTeX mathematical
;; expressions.
;;  $ ... $
;;  $$ ... $$
;;  \[ ... \]
;;  \begin{math} ... \end{math}
;;  \begin{displaymath} ... \end{displaymath}
;;  \begin{equation} ... \end{equation}
;;  \begin{gather} ... \end{gather}
;;  \begin{align} ... \end{align}
;;  \begin{alignat} ... \end{alignat}

;;; Keymap:
;; In preview window, the following binded key is applicable.
;;  q: exit preview buffer
;;  Q: delete preview buffer
;;  j: scroll up
;;  k: scroll down

;; ChangeLog:
;; 2009/07/31 version 0.1.1 yamaguchi
;;     adjust background and foreground colors of png to default face.
;; 2009/07/25 version 0.1.0 yamaguchi
;;     support Meadow3.

;;; Code:

(require 'cl)
(require 'thingatpt)

;;;###autoload
(defgroup latex-math-preview nil
  "LaTeX Math Preview."
 :prefix "latex-math-preview-"
 :group 'applications
 )

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

(defvar latex-math-preview-candidates-buffer-name
  "*latex-math-preview-candidates*"
  "Name of buffer which displays candidates of LaTeX symbols.")

(defvar latex-math-preview-latex-command
  "latex" "Path to latex.")

(defvar latex-math-preview-command-dvipng
  "dvipng" "Path to dvipng.")

(defvar latex-math-preview-command-dvi-view
  "xdvi" "Path to dvi viewer.")

(defvar latex-math-preview-temporary-file-prefix
  "temp_latex_math"
  "The prefix name of some temporary files which is produced in making an image.")

(defvar latex-math-preview-cache-directory
  (concat (getenv "HOME") "/.emacs.d/latex-math-preview-cache")
  "Cache directory.")

(defvar latex-math-preview-dvipng-option
  '("-x" "1728" "-T" "tight")
  "Option for dvipng.")

(defvar latex-math-preview-image-foreground-color nil
  "Foreground color of image created by dvipng.")

(defvar latex-math-preview-image-background-color nil
  "Background color of image created by dvipng.")

(defvar latex-math-preview-dvipng-color-option nil
  "Temporary variable. You must not set this variable.")

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

    )
  "These eqpressions are used for matching to extract tex math expression.")

(defvar latex-math-preview-insertion-candidates
  '(
    ("delimiters" .
     ("\\lfloor" "\\rfloor" "\\lceil" "\\rceil" "\\langle" "\\rangle"
      "\\backslash" "\\|" "\\uparrow" "\\Uparrow" "\\downarrow" "\\Downarrow"
      "\\updownarrow" "\\Updownarrow"))
    ("greek-letters" .
     ("\\alpha" "\\beta" "\\gamma" "\\delta" "\\epsilon" "\\zeta" "\\eta"
      "\\theta" "\\iota" "\\kappa" "\\lambda" "\\mu" "\\nu" "\\xi" "\\pi"
      "\\rho" "\\sigma" "\\tau" "\\upsilon" "\\phi" "\\chi" "\\psi" "\\omega"
      "\\varepsilon" "\\vartheta" "\\varpi" "\\varrho" "\\varsigma" "\\varphi"
      "\\Gamma" "\\Delta" "\\Theta" "\\Lambda" "\\Xi" "\\Pi" "\\Sigma"
      "\\Upsilon" "\\Phi" "\\Psi" "\\Omega"))
    ("binary-operators" .
     ("\\pm" "\\mp" "\\times" "\\div"  "\\ast" "\\star" "\\circ" "\\bullet"
      "\\cdot" "\\cap" "\\cap" "\\uplus" "\\sqcap" "\\sqcup" "\\vee" "\\wedge"
      "\\setminus" "\\wr" "\\diamond" "\\bigtriangleup" "\\bigtriangledown"
      "\\triangleleft" "\\triangleright" "\\oplus" "\\ominus" "\\otimes"
      "\\oslash" "\\odot" "\\bigcirc" "\\dagger" "\\ddagger" "\\amalg"))
    ("relational-operators" .
     ("\\le" "\\prec" "\\preceq" "\\ll" "\\subset" "\\subseteq" "\\vdash"
      "\\in" "\\notin" "\\ge" "\\succ" "\\succeq" "\\gg" "\\supset" "\\supseteq"
      "\\sqsupseteq" "\\dashv" "\\ni"
      ;; omit "\\leq" and "geq" because this is the same as "\\le" and "\\ge", respectively.
      "\\equiv" "\\sim" "\\simeq" "\\asymp" "\\approx" "\\cong" "\\neq"
      "\\doteq" "\\propto" "\\models" "\\perp" "\\mid" "\\parallel" "\\bowtie"
      "\\smile" "\\frown"))
    ("arrows" .
     ("\\leftarrow" "\\gets" "\\Leftarrow" "\\rightarrow" "\\to" "\\Rightarrow"
      "\\leftrightarrow" "\\Leftrightarrow" "\\mapsto" "\\hookleftarrow"
      "\\leftharpoonup" "\\leftharpoondown" "\\longleftarrow" "\\Longleftarrow"
      "\\longleftrightarrow" "\\Longleftrightarrow" "\\longmapsto"
      "\\hookrightarrow" "\\rightharpoonup" "\\rightharpoondown"
      "\\iff" "\\nearrow" "\\searrow" "\\swarrow" "\\nwarrow"
      "\\rightleftharpoons"))
    ("miscellaneous" .
     ("\\aleph" "\\hbar" "\\imath" "\\jmath" "\\ell" "\\wp" "\\Re" "\\Im"
      "\\partial" "\\infty" "\\prime" "\\emptyset" "\\nabla" "\\surd"
      "\\top" "\\bot" "\\angle" "\\triangle" "\\forall" "\\exists"
      "\\neg" "\\flat" "\\natural" "\\sharp" "\\clubsuit" "\\diamondsuit"
      "\\heartsuit" "\\spadesuit"))
    ("functions" .
     ("\\arccos" "\\arcsin" "\\arctan" "\\arg" "\\cos" "\\cosh" "\\cot" "\\coth"
      "\\csc" "\\deg" "\\det" "\\dim" "\\exp" "\\gcd" "\\hom" "\\inf" "\\ker"
      "\\lg" "\\lim" "\\liminf" "\\limsup" "\\ln" "\\log" "\\max" "\\min"
      "\\Pr" "\\sec" "\\sin" "\\sinh" "\\sup" "\\tan" "\\tanh"
      "\\bmod" "\\pmod"))
    ("accessories" .
     (("\\hat{" "a" "}") ("\\check{" "a" "}") ("\\breve{" "a" "}")
      ("\\acute{" "a" "}") ("\\grave{" "a" "}") ("\\tilde{" "a" "}")
      ("\\bar{" "a" "}") ("\\vec{" "a" "}") ("\\dot{" "a" "}") ("\\ddot{" "a" "}")
      ("\\overline{" "x+y" "}") "\\underline{x+y}" ("\\widehat{" "xyz" "}")
      ("\\widetilde{" "xyz" "}") ("\\overbrace{" "x+y" "}")
      ("\\underbrace{" "x+y" "}") ("\\overrightarrow{" "\\mathrm{OA}" "}")
      ("\\overleftarrow{" "\\mathrm{OA}" "}")))
    ("typefaces" .
     (("\\mathbf{" "x" "}") ("\\mathit{" "diff" "}") ("\\mathrm{" "H" "}") ("\\mathcal{" "H" "}")
      ("\\mathsf{" "H" "}") ("\\mathtt{" "H" "}")))
    
    ;; ("others" .
    ;;  ("\\vartriangleleft" "\\vartriangleright" "\\trianglelefteq" "\\trianglerighteq"
    ;;   "\\sqsubset" "\\sqsupset" "\\Join" "\\rightsquigarrow" "\\square"
    ;;   "\\lozenge" "\\mho"))
    )
  "List of candidates for LaTeX symbol insertion.")

(defvar latex-math-preview-window-configuration nil
  "Temporary variable in which window configuration is saved.")

(defvar latex-math-preview-candidates-defined-as-list nil
  "Temporary variable.")
(setq latex-math-preview-candidates-defined-as-list nil)

(defvar latex-math-preview-not-delete-tmpfile nil
  "Not delete temporary files and directory if this value is true. Mainly for debugging.")

(defvar latex-math-preview-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'latex-math-preview-exit-window)
    (define-key map (kbd "Q") 'latex-math-preview-delete-buffer)
    (define-key map (kbd "j") 'scroll-up)
    (define-key map (kbd "k") 'scroll-down)
    (define-key map (kbd "n") 'scroll-up)
    (define-key map (kbd "p") 'scroll-down)
    map)
  "Keymap for latex-math-preview.")

(defvar latex-math-preview-insertion-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'latex-math-preview-exit-window)
    (define-key map (kbd "Q") 'latex-math-preview-delete-buffer)
    (define-key map (kbd "j") 'scroll-up)
    (define-key map (kbd "k") 'scroll-down)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "\C-m") 'latex-math-preview-insert-candidate)
    (define-key map (kbd "<return>") 'latex-math-preview-insert-candidate)
    map)
  "Keymap for insertion mode of latex-math-preview.")

;;-----------------------------------------------------------------------------

(defun latex-math-preview-bounds-of-latex-math ()
  "A `bounds-of-thing-at-point' function for a LaTeX mathematical expression.
See `latex-math-preview-match-expression' for what's matched.
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
The `latex-math-preview-function' variable controls the viewing method. 
The LaTeX notations which can be matched are $...$, $$...$$ or
the notations which are stored in `latex-math-preview-match-expression'."

  (interactive)
  (let ((str (thing-at-point 'latex-math)))
    (or str
        (error "Not in a TeX math expression"))
    (setq latex-math-preview-window-configuration (current-window-configuration))
    (latex-math-preview-str str)))

(defun latex-math-preview-make-dvi-file (tmpdir math-exp)
  "Make temporary tex file including MATH-EXP in TMPDIR and compile it."
  (let ((dot-tex (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".tex"))
	(dot-dvi (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".dvi")))
    (with-temp-file dot-tex
      (insert latex-math-preview-latex-template-header)
      (insert math-exp)
      (insert latex-math-preview-latex-template-footer))
    (if (not (eq 0 (call-process latex-math-preview-latex-command nil nil nil
				 (concat "-output-directory=" latex-math-dir) dot-tex)))
	(error "TeX processing error")
      dot-dvi)
    ))

(defun latex-math-preview-clear-tmp-directory (dir)
  "Delete temporary directory and files contained in it."
  (if (file-directory-p dir)
      (progn
	(let ((directories))
	  (dolist (file (directory-files dir))
	    (let ((path (concat dir "/" file)))
	      (cond ((and (file-directory-p path) (not (string-match "^\\.+$" file)))
		     (add-to-list 'directories file))
		    ((file-regular-p path)
		     (condition-case nil (delete-file path)
		       (error (message "Can not delete '%s'" path)))))))
	  (dolist (del-dir directories)
	    (message del-dir)
	    (latex-math-preview-clear-tmp-directory (concat dir "/" del-dir))))
	(condition-case nil (delete-directory dir)
	  (error (message "Can not delete '%s'" dir))))))

(defun latex-math-preview-str (str)
  "Preview the given STR string as a TeX math expression.
STR should not have $ or $$ delimiters."

  (let ((latex-math-dir (make-temp-file "latex-math-preview-" t)))
    (funcall latex-math-preview-function (latex-math-preview-make-dvi-file latex-math-dir str))
    (if (not latex-math-preview-not-delete-tmpfile)
	;; cleanup temp files
	(latex-math-preview-clear-tmp-directory latex-math-dir)
      )))

;;-----------------------------------------------------------------------------
;; adaptive viewer selection

(defun latex-math-preview-adaptview (filename)
  "Display dvi FILENAME using either png image or
`latex-math-preview-command-dvi-view'.
A PNG image in a buffer per `latex-math-preview-png-image' is used
if possible, or if not then the `tex-mode' previewer given by
`latex-math-preview-command-dvi-view'.

This function is the default for `latex-math-preview-function',
allowing `latex-math-preview' to adapt to the Emacs display
capabilities and available viewer program(s)."

  (if (and (image-type-available-p 'png)
           (display-images-p)
           (eq 0 (call-process shell-file-name nil nil nil "-c" latex-math-preview-command-dvipng "--version")))
      (latex-math-preview-png-image filename)
      (latex-math-preview-dvi-view filename)))

;;-----------------------------------------------------------------------------
;; view by running tex-dvi-view-command

(defun latex-math-preview-dvi-view (filename)
  "Display dvi FILENAME using `latex-math-preview-command-dvi-view'."
  (message filename)
  (call-process latex-math-preview-command-dvi-view nil nil nil filename))

;;-----------------------------------------------------------------------------
;; view png in a buffer

(defun latex-math-preview-png-image (dvifile)
  "Display dvi DVIFILE as a png image in a buffer.
This can be used in `latex-math-preview-function', but it requires:

* the \"dvipng\" program (http://sourceforge.net/projects/dvipng/)
* a display which can show images (eg. X, not a tty)
* Emacs built with the PNG image libraries"

  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (let ((image (latex-math-preview-dvi-to-png dvifile)))
    (if image
	(progn
	  (with-current-buffer (get-buffer-create latex-math-preview-buffer-name)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (insert " ")
	    (insert-image-file image)
	    (end-of-line)
	    (insert "\n")
	    (goto-char (point-min))
	    (buffer-disable-undo)
	    (setq buffer-read-only t)
	    (use-local-map latex-math-preview-map)
	    (setq mode-name "LaTeXPreview")
	    )
	  (pop-to-buffer latex-math-preview-buffer-name)
	  ))))

(defun latex-math-preview-get-dvipng-color-option()
  (let ((max (car (color-values "#ffffff"))))
    (list (concat "-bg rgb "
		  (mapconcat
		   (lambda (col) (number-to-string (/ (float col) max)))
		   (color-values (or latex-math-preview-image-background-color (face-background 'default))) " "))
	  (concat "-fg rgb "
		  (mapconcat
		   (lambda (col) (number-to-string (/ (float col) max)))
		   (color-values (or latex-math-preview-image-foreground-color (face-foreground 'default))) " ")))
    ))

(defun latex-math-preview-dvi-to-png (filename &optional output)
  "Render dvi FILENAME to an Emacs image and return that.
The \"dvipng\" program is used for drawing.  If it fails a shell
buffer is left showing the messages and the return is nil."

  (if (not latex-math-preview-dvipng-color-option)
      (setq latex-math-preview-dvipng-color-option (latex-math-preview-get-dvipng-color-option)))
  (let ((dot-png (or output (concat latex-math-dir "/" latex-math-preview-temporary-file-prefix ".png"))))
    (if (eq 0 (eval `(call-process latex-math-preview-command-dvipng nil nil nil "-o" dot-png
				   ,@latex-math-preview-dvipng-option ,@latex-math-preview-dvipng-color-option filename)))
	dot-png
      nil)))

;;-----------------------------------------------------------------------------
;; Manage window

(defun latex-math-preview-exit-window ()
  "Exit preview window."
  (interactive)
  (set-window-configuration latex-math-preview-window-configuration))

(defun latex-math-preview-delete-buffer ()
  "Delete buffer of preview"
  (interactive)
  (kill-buffer (buffer-name))
  (set-window-configuration latex-math-preview-window-configuration))

; Clear dvipng option for coloring.
(setq latex-math-preview-dvipng-color-option nil)

;;-----------------------------------------------------------------------------
;; Insert Mathematical expression 

(defun latex-math-preview-clear-cache (&optional dirname)
  (interactive)
  (if dirname
      (if (assoc dirname latex-math-preview-insertion-candidates)
	  ((latex-math-preview-clear-tmp-directory
	    (concat latex-math-preview-cache-directory "/" dirname))))
    (latex-math-preview-clear-tmp-directory latex-math-preview-cache-directory)))

(defun latex-math-preview-make-cache-images (dirname)
  (let ((latex-syms (cdr (assoc dirname latex-math-preview-insertion-candidates)))
	(dirpath (concat latex-math-preview-cache-directory "/" dirname))
	(num 0))
    (if (file-directory-p dirpath)
	(message "'%s' exists. Cache may be used." dirpath)
      (progn
	(make-directory dirpath t)
	(message "Creating images. Please wait for a while.")
	(dolist (sym latex-syms)
	  (if (listp sym)
	      (latex-math-preview-make-insertion-image (eval `(concat ,@sym)) dirname num)
	    (latex-math-preview-make-insertion-image sym dirname num))
	  (setq num (+ num 1)))
	))))

(defun latex-math-preview-make-insertion-image (math-symbol dirname num)
  (let ((latex-math-dir (make-temp-file "latex-math-preview-" t))
	(path (concat latex-math-preview-cache-directory "/" dirname "/"
		      (format "%05d" num) "_"
		      (downcase (replace-regexp-in-string "\\(\\\\\\)\\|\\({\\)\\|\\(}\\)"
							  "_" math-symbol)) ".png")))
    (latex-math-preview-dvi-to-png
     (latex-math-preview-make-dvi-file latex-math-dir (concat "$" math-symbol "$")) path)
    (latex-math-preview-clear-tmp-directory latex-math-dir)
    path))

(defun latex-math-preview-insertion-candidate-buffer (dirname)
  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (latex-math-preview-make-cache-images dirname)
  (setq latex-math-preview-window-configuration (current-window-configuration))
  (with-current-buffer (get-buffer-create latex-math-preview-candidates-buffer-name)
    (setq truncate-lines t)
    (setq tab-width 8)
    (setq line-spacing 8)
    (setq buffer-read-only nil)
    (erase-buffer)
    (let* ((latex-symbols (cdr (assoc dirname latex-math-preview-insertion-candidates)))
	  (dirpath (concat latex-math-preview-cache-directory "/" dirname))
	  (max-tab (/ (window-width) tab-width))
	  (num max-tab))
      (dolist (pngfile (sort (delete-if (lambda (file) (not (string-match "^[0-9]+_" file)))
				    (directory-files dirpath)) 'string<))
	(insert-image (create-image (concat latex-math-preview-cache-directory "/" dirname "/" pngfile)
				    'png nil :ascent 'center))
	(end-of-line)
	(let ((sym (car latex-symbols)) (str))
	  (if (listp sym)
	      (progn
		(setq str (eval `(concat ,@sym)))
		  (add-to-list 'latex-math-preview-candidates-defined-as-list `(,str ,sym)))
	    (setq str sym))
	  (insert "\t" (format "%-30s" str)))
	(setq latex-symbols (cdr latex-symbols))
	(setq num (- num 2))
	(if (< num 2)
	    (progn
	      (insert "\n")
	      (setq num max-tab))
	  (insert "\t"))))
    (goto-char (point-min))
    (buffer-disable-undo)
    (setq buffer-read-only t)
    (use-local-map latex-math-preview-insertion-map)
    (setq mode-name "LaTeXPreview"))
  (pop-to-buffer latex-math-preview-candidates-buffer-name))

(defun latex-math-preview-insert-candidate ()
  (interactive)
  (let ((str) (sym))
    (save-excursion
      (skip-chars-backward "^\n\r ")
      (if (re-search-forward "\t\\([^\t]*\\)\t\\|$" nil t)
	  (setq str (buffer-substring (match-beginning 1) (match-end 1)))))
    (message str)
    (setq sym (assoc str latex-math-preview-candidates-defined-as-list))
    (latex-math-preview-exit-window)
    (if sym
	(progn
	  (insert (car (car (cdr sym))))
	  (save-excursion
	    (insert (car (cdr (cdr (car (cdr sym))))))))
      (insert str))))

(defun latex-math-preview-insertion (&optional dirname)
  (interactive)
  (if (not dirname)
      (setq dirname (completing-read "type: " latex-math-preview-insertion-candidates nil t)))
  (latex-math-preview-insertion-candidate-buffer dirname))

(provide 'latex-math-preview)

;;; latex-math-preview.el ends here
