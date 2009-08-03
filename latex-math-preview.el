;;; latex-math-preview.el --- preview LaTeX mathematical expressions.

;; Author: Takayuki YAMAGUCHI <d@ytak.info>
;; Keywords: LaTeX TeX
;; Version: 0.2.2
;; Created: Mon Aug  3 00:02:43 2009

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
;; M-x latex-math-preview-expression previews mathematical expressions pointed
;; by cursor in LaTeX files.
;; The result of latex-math-preview-expression is shown in new buffer as image
;; or as dvi file by dvi viewer.
;; 
;; M-x latex-math-preview-insert-symbol displays list of mathematical symbols.
;; You can insert a LaTeX mathematical symbol from it.

;; Requirements;
;; Emacs (version 22 or 23) on Linux or Meadow3 on Windows.
;; dvipng
;; latex or platex
;; dvi viewer (for example, xdvi) if needed

;;; Install:
;; Put latex-math-preview.el to your load-path and
;; write the following code in ~/.emacs.el.
;; 
;;   (autoload 'latex-math-preview-expression "latex-math-preview" nil t)
;;   (autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
;; 
;; For YaTeX mode, add the follwing to ~/.emacs.el if desired.
;;
;;   (add-hook 'yatex-mode-hook
;;            '(lambda ()
;; 	      (YaTeX-define-key "p" 'latex-math-preview-expression)
;; 	      (YaTeX-define-key "j" 'latex-math-preview-insert-symbol)
;;            (YaTeX-define-key "\C-j" 'latex-math-preview-last-symbol-again)))
;;
;; This setting almost binds latex-math-preview-expression to "C-c p"
;; and latex-math-preview-insert-symbol to "C-c j".

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
;; latex-math-preview-expression makes a temporary latex file and compiles it and 
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
;; \usepackage{amsmath, amssymb, amsthm}
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
;; latex-math-preview-expression compiles a file like the above and
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
;;     New function `latex-math-preview-last-symbol-again'.
;; 2009/08/03 version 0.2.2 yamaguchi
;;     Change name of function `latex-math-preview-insert-sign' to `latex-math-preview-insert-symbol'.
;;     Add some groups of symbol candidates.
;;     Some bug fixes.
;; 2009/08/02 version 0.2.1 yamaguchi
;;     New command latex-math-preview-next-candidates-for-insertion.
;;     Some adjustments.
;; 2009/08/02 version 0.2.0 yamaguchi
;;     New command latex-math-preview-insert-sign.
;;     Change name of function `latex-math-preview' to `latex-math-preview-expression'.
;;     bug fixes.
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
  "Function for `latex-math-preview-expression' to show a DVI file.
The default `latex-math-preview-adaptview' chooses among the
methods, according to what Emacs and the system supports."
  :type '(choice (latex-math-preview-adaptview
                  latex-math-preview-dvi-view
                  latex-math-preview-png-image)
                 function)
  :group 'latex-math-preview)

(defvar latex-math-preview-expression-buffer-name
  "*latex-math-preview*"
  "Name of buffer which displays preview image.")

(defvar latex-math-preview-insert-symbol-buffer-name
  "*latex-math-preview-candidates*"
  "Name of buffer which displays candidates of LaTeX mathematical symbols.")

(defvar latex-math-preview-latex-command
  "latex" "Path to latex.")

(defvar latex-math-preview-command-dvipng
  "dvipng" "Path to dvipng.")

(defvar latex-math-preview-command-dvi-view
  "xdvi" "Path to dvi viewer.")

(defvar latex-math-preview-temporary-file-prefix
  "temp_latex_math"
  "The prefix name of some temporary files which is produced in making an image.")

(defvar latex-math-preview-cache-directory-for-insertion
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
  "\\documentclass{article}\n\\usepackage{amsmath, amssymb, amsthm}\n\\pagestyle{empty}\n\\begin{document}\n"
  "Insert string to beggining of temporary latex file to make image.")

(defvar latex-math-preview-latex-template-footer
  "\\par\n\\end{document}\n"
  "Insert string to end of temporary latex file to make image.")

(defvar latex-math-preview-window-configuration nil
  "Temporary variable in which window configuration is saved.")

(defvar latex-math-preview-candidates-defined-as-list nil
  "Temporary variable.")

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

(defvar latex-math-preview-candidates-for-insertion
  '(
    ("delimiters" .
     (("(" "x" ")") ("[" "x" "]") ("\\{" "x" "\\}")
      ("\\lfloor " "x" " \\rfloor") ("\\lceil " "x" " \\rceil")
      ("\\langle " "x" " \\rangle")
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
      "\\cdot" "\\cap" "\\cup" "\\uplus" "\\sqcap" "\\sqcup" "\\vee" "\\wedge"
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
      "\\smile" "\\frown"
      "\\not\\equiv"))
    ("arrows" .
     ("\\gets" "\\Leftarrow" "\\to" "\\Rightarrow"
      ;; omit "\\leftarrow" and "\\rightarrow"
      "\\leftrightarrow" "\\Leftrightarrow" "\\mapsto" "\\hookleftarrow"
      "\\leftharpoonup" "\\leftharpoondown" "\\longleftarrow" "\\Longleftarrow"
      "\\longleftrightarrow" "\\Longleftrightarrow" "\\longmapsto"
      "\\hookrightarrow" "\\rightharpoonup" "\\rightharpoondown"
      "\\iff" "\\nearrow" "\\searrow" "\\swarrow" "\\nwarrow"
      "\\rightleftharpoons"))
    ("miscellaneous-symbols" .
     ("\\aleph" "\\hbar" "\\imath" "\\jmath" "\\ell" "\\wp" "\\Re" "\\Im"
      "\\partial" "\\infty" "\\prime" "\\emptyset" "\\nabla" "\\surd"
      "\\top" "\\bot" "\\angle" "\\triangle" "\\forall" "\\exists"
      "\\neg" "\\flat" "\\natural" "\\sharp" "\\clubsuit" "\\diamondsuit"
      "\\heartsuit" "\\spadesuit"))
    ("big-symbols" .
     ("\\sum" "\\prod" "\\coprod" "\\int" "\\oint" "\\bigcap" "\\bigcup"
      "\\bigsqcup" "\\bigvee" "\\bigwedge" "\\bigodot" "\\bigotimes"
      "\\bigoplus" "\\biguplus"))
    ("functions" .
     ("\\arccos" "\\arcsin" "\\arctan" "\\arg" "\\cos" "\\cosh" "\\cot" "\\coth"
      "\\csc" "\\deg" "\\det" "\\dim" "\\exp" "\\gcd" "\\hom" "\\inf" "\\ker"
      "\\lg" "\\lim" "\\liminf" "\\limsup" "\\ln" "\\log" "\\max" "\\min"
      "\\Pr" "\\sec" "\\sin" "\\sinh" "\\sup" "\\tan" "\\tanh"
      "\\bmod" "\\pmod"))
    ("attachments-and-others" .
     (("\\hat{" "a" "}") ("\\check{" "a" "}") ("\\breve{" "a" "}")
      ("\\acute{" "a" "}") ("\\grave{" "a" "}") ("\\tilde{" "a" "}")
      ("\\bar{" "a" "}") ("\\vec{" "a" "}") ("\\dot{" "a" "}") ("\\ddot{" "a" "}")
      ("\\overline{" "xy" "}") "\\underline{xy}" ("\\widehat{" "xy" "}")
      ("\\widetilde{" "xy" "}") ("\\overbrace{" "xy" "}")
      ("\\underbrace{" "xy" "}") ("\\overrightarrow{" "\\mathrm{OA}" "}")
      ("\\overleftarrow{" "\\mathrm{OA}" "}") ("\\stackrel{" "f" "}{\\to}")
      "\\stackrel{\\mathrm{def}}{=}"
      ("\\frac{" "x" "}{y}") ("" "x" "^{n}") "\\sum_{i=0}^{\\infty}"
      ("\\sqrt{" "x" "}") ("\\sqrt[" "3" "]{x}")))
    ("typefaces" .
     (("\\mathrm{" "abcdeABCDE" "}") ("\\mathbf{" "abcdeABCDE" "}")
      ("\\mathit{" "abcdeABCDE" "}") ("\\mathcal{" "ABCDE" "}")
      ("\\mathsf{" "abcdeABCDE" "}") ("\\mathtt{" "abcdeABCDE" "}")))
    ("AMSFonts-binary-operators" .
     ("\\boxdot" "\\boxplus" "\\centerdot" "\\boxminus" "\\veebar" "\\barwedge"
      "\\doublebarwedge" "\\Cup" "\\Cap" "\\curlywedge" "\\curlyvee"
      "\\leftthreetimes" "\\rightthreetimes" "\\dotplus" "\\intercal"
      "\\circledcirc" "\\circledast" "\\circleddash" "\\divideontimes" "\\lessdot"
      "\\gtrdot" "\\ltimes" "\\rtimes" "\\smallsetminus"))
    ("AMSFonts-relational-operators1" .
     ("\\circlearrowright" "\\circlearrowleft" "\\rightleftharpoons"
      "\\leftrightharpoons" "\\Vdash" "\\Vvdash" "\\vDash" "\\twoheadrightarrow"
      "\\twoheadleftarrow" "\\leftleftarrows" "\\rightrightarrows" "\\upuparrows"
      "\\downdownarrows" "\\upharpoonright" "\\downharpoonright" "\\upharpoonleft"
      "\\downharpoonleft" "\\rightarrowtail" "\\leftarrowtail" "\\rightleftarrows"
      "\\Lsh" "\\Rsh" "\\rightsquigarrow" "\\leftrightsquigarrow" "\\looparrowleft"
      "\\looparrowright" "\\circeq" "\\succsim" "\\gtrsim" "\\gtrapprox"
      "\\multimap" "\\therefore" "\\because" "\\doteqdot" "\\triangleq" "\\precsim" 
      "\\lesssim" "\\lessapprox" "\\eqslantless" "\\eqslantgtr" "\\curlyeqprec"
      "\\curlyeqsucc"))
    ("AMSFonts-relational-operators2" .
     ("\\preccurlyeq" "\\leqq" "\\leqslant" "\\lessgtr" "\\risingdotseq"
      "\\fallingdotseq" "\\succcurlyeq" "\\geqq" "\\geqslant" "\\gtrless"
      "\\sqsubset" "\\sqsupset" "\\vartriangleright" "\\vartriangleleft"
      "\\trianglerighteq" "\\trianglelefteq" "\\between" "\\blacktriangleright"
      "\\blacktriangleleft" "\\vartriangle" "\\eqcirc" "\\lesseqgtr" "\\gtreqless"
      "\\lesseqqgtr" "\\gtreqqless" "\\Rrightarrow" "\\Lleftarrow" "\\varpropto"
      "\\smallsmile" "\\smallfrown" "\\Subset" "\\Supset" "\\subseteqq"
      "\\supseteqq" "\\bumpeq" "\\Bumpeq" "\\lll" "\\ggg" "\\pitchfork"
      "\\backsim" "\\backsimeq"))
    ("AMSFonts-relational-operators3" .
     ("\\lvertneqq" "\\gvertneqq" "\\nleq" "\\ngeq" "\\nless" "\\ngtr" "\\nprec"
      "\\nsucc" "\\lneqq" "\\gneqq" "\\nleqslant" "\\ngeqslant" "\\lneq" "\\gneq"
      "\\npreceq" "\\nsucceq" "\\precnsim" "\\succnsim" "\\lnsim" "\\gnsim"
      "\\nleqq" "\\ngeqq" "\\precneqq" "\\succneqq" "\\precnapprox" "\\succnapprox"
      "\\lnapprox" "\\gnapprox" "\\nsim" "\\ncong" "\\varsubsetneq" "\\varsupsetneq"
      "\\nsubseteqq" "\\nsupseteqq" "\\subsetneqq" "\\supsetneqq" "\\varsubsetneqq"
      "\\varsupsetneqq" "\\subsetneq" "\\supsetneq" "\\nsubseteq" "\\nsupseteq"
      "\\nparallel" "\\nmid" "\\nshortmid" "\\nshortparallel" "\\nvdash"
      "\\nVdash" "\\nvDash" "\\nVDash" "\\ntrianglerighteq" "\\ntrianglelefteq"
      "\\ntriangleleft" "\\ntriangleright" "\\nleftarrow" "\\nLeftarrow"
      "\\nRightarrow" "\\nLeftrightarrow" "\\nleftrightarrow" "\\eqsim"
      "\\shortmid" "\\shortparallel" "\\thicksim" "\\thickapprox" "\\approxeq"
      "\\succapprox" "\\precapprox" "\\curvearrowleft" "\\curvearrowright"
      "\\backepsilon"))
    ("AMSFonts-other-symbols" .
     ("\\square" "\\blacksquare" "\\lozenge" "\\blacklozenge" "\\backprime"
      "\\bigstar" "\\blacktriangledown" "\\blacktriangle" "\\triangledown"
      "\\angle" "\\measuredangle" "\\sphericalangle" "\\circledS" "\\complement"
      "\\diagup" "\\diagdown" "\\varnothing" "\\nexists" "\\Finv" "\\Game"
      "\\mho" "\\eth" "\\beth" "\\gimel" "\\daleth" "\\digamma"
      "\\varkappa" "\\Bbbk" "\\hslash" "\\hbar"))
    ("AMSFonts-italic-greeks" .
     ("\\varGamma" "\\varDelta" "\\varTheta" "\\varLambda" "\\varXi" "\\varPi"
      "\\varSigma" "\\varUpsilon" "\\varPhi" "\\varPsi" "\\varOmega"))
    ("AMSFonts-others" .
     (("\\mathfrak{" "ABCDE" "}") ("\\mathbb{" "ABCDE" "}")
      "\\dots" "\\dotsc" "\\dotsb" "\\dotsm" "\\dotsi"
      ("\\overleftrightarrow{" "A" "}") ("\\underleftrightarrow{" "A" "}")
      ("\\xrightarrow{" "\\text{text}" "}") ("\\xrightarrow[" "abc" "]{}")
      ("\\xleftarrow{" "\\text{text}" "}") ("\\xleftarrow[" "abc" "]{}")
      ("\\Hat{" "A" "}") ("\\Dot{" "A" "}") ("\\Check{" "A" "}")
      ("\\Ddot{" "A" "}") ("\\Tilde{" "A" "}") ("\\Breve{" "A" "}")
      ("\\Acute{" "A" "}") ("\\Bar{" "A" "}") ("\\Vec{" "A" "}")
      ("\\dddot{" "x" "}") ("\\ddddot{" "x" "}")
      "\\int" "\\iint" "\\iiint" "\\iiiint" "\\idotsint"
      ("\\tfrac{" "a" "}{b}") ("\\dfrac{" "a" "}{b}") ("\\cfrac{" "a" "}{b}")
      ("\\binom{" "a" "}{b}")))
    )
  "List of candidates for insertion of LaTeX mathematical symbol.")

(defvar latex-math-preview-restore-symbol-group t
  "Restore last symbol group at next insertion
 if this `latex-math-preview-restore-symbol-group' is non-nil.")

(defvar latex-math-preview-inserted-last-symbol nil
  "Inserted last symbol.")

(defvar latex-math-preview-initial-group
  (car (nth 0 latex-math-preview-candidates-for-insertion))
  "Group which is displayed initially.")

(defvar latex-math-preview-current-group nil
  "Group of present buffer displaying mathematical symbols.")

(defvar latex-math-preview-number-start-candidates 0
  "Temporary variable which is line number starting display of candidates.")

(defface latex-math-preview-candidate-for-insertion-face
  '((t (:foreground "dark orange")))
  "Face for notations of LaTeX mathematical symbol.")

(defface latex-math-preview-key-for-insertion-face
  '((t (:foreground "dodger blue")))
  "Face for notations of LaTeX mathematical symbol.")
  
(defvar latex-math-preview-selection-face-for-insertion 'highlight
  "Face for currently selected item.")

(defvar latex-math-preview-selection-overlay-for-insertion nil
  "Overlay for highlighting currently selected item.")

(defvar latex-math-preview-not-delete-tmpfile nil
  "Not delete temporary files and directory if this value is true. Mainly for debugging.")

(defvar latex-math-preview-dvipng-log-buffer nil
  "Buffer name for output by dvipng. Mainly for debugging.")

(defvar latex-math-preview-in-math-mode-p-func 'latex-math-preview-in-math-mode-p
  "Symbol of function is used for determining whether cursor is in mathematical expression.
If you use YaTeX mode then the recommended value of this variable is YaTeX-in-math-mode-p.")

(defvar latex-math-preview-expression-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'latex-math-preview-quit-window)
    (define-key map (kbd "Q") 'latex-math-preview-delete-buffer)
    (define-key map (kbd "\C-g") 'latex-math-preview-quit-window)
    (define-key map (kbd "j") 'scroll-up)
    (define-key map (kbd "k") 'scroll-down)
    (define-key map (kbd "n") 'scroll-up)
    (define-key map (kbd "p") 'scroll-down)
    map)
  "Keymap for latex-math-preview-expression.")

(defvar latex-math-preview-insert-symbol-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'latex-math-preview-quit-window)
    (define-key map (kbd "Q") 'latex-math-preview-delete-buffer)
    (define-key map (kbd "\C-g") 'latex-math-preview-quit-window)
    (define-key map (kbd "o") 'delete-other-windows)
    (define-key map (kbd "c") 'latex-math-preview-symbols-of-other-group)
    (define-key map (kbd "j") 'latex-math-preview-move-to-downward-item)
    (define-key map (kbd "k") 'latex-math-preview-move-to-upward-item)
    (define-key map (kbd "l") 'latex-math-preview-move-to-right-item)
    (define-key map (kbd "h") 'latex-math-preview-move-to-left-item)
    (define-key map (kbd "n") 'latex-math-preview-move-to-downward-item)
    (define-key map (kbd "p") 'latex-math-preview-move-to-upward-item)
    (define-key map (kbd "f") 'latex-math-preview-move-to-right-item)
    (define-key map (kbd "b") 'latex-math-preview-move-to-left-item)
    (define-key map (kbd "<down>") 'latex-math-preview-move-to-downward-item)
    (define-key map (kbd "<up>") 'latex-math-preview-move-to-upward-item)
    (define-key map (kbd "<right>") 'latex-math-preview-move-to-right-item)
    (define-key map (kbd "<left>") 'latex-math-preview-move-to-left-item)
    (define-key map (kbd ".") 'latex-math-preview-next-candidates-for-insertion)
    (define-key map (kbd ",") 'latex-math-preview-previous-candidates-for-insertion)
    (define-key map (kbd "\C-n") 'latex-math-preview-next-candidates-for-insertion)
    (define-key map (kbd "\C-p") 'latex-math-preview-previous-candidates-for-insertion)
    (define-key map (kbd "\C-m") 'latex-math-preview-put-selected-candidate)
    (define-key map (kbd "<return>") 'latex-math-preview-put-selected-candidate)
    map)
  "Keymap for insertion mode of latex-math-preview-insert-symbol.")

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

(defun latex-math-preview-in-math-mode-p ()
  "Return non-nil if current position is in mathematical expression.
This function may make mistake when there is sequence of '$'.
If you use YaTeX, then you should use YaTeX-in-math-mode-p alternatively."
  (thing-at-point 'latex-math))

;;;###autoload
(defun latex-math-preview-expression ()
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
allowing `latex-math-preview-expression' to adapt to the Emacs display
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
	  (with-current-buffer (get-buffer-create latex-math-preview-expression-buffer-name)
	    (setq cursor-type nil)
	    (setq buffer-read-only nil)
	    (erase-buffer)
	    (insert " ")
	    (insert-image-file image)
	    (end-of-line)
	    (insert "\n")
	    (goto-char (point-min))
	    (buffer-disable-undo)
	    (setq buffer-read-only t)
	    (use-local-map latex-math-preview-expression-map)
	    (setq mode-name "LaTeXPreview")
	    )
	  (pop-to-buffer latex-math-preview-expression-buffer-name)
	  ))))

(defun latex-math-preview-get-dvipng-color-option ()
  "Get string for dvipng options '-bg' and '-fg'."
  (let ((max (car (color-values "#ffffff"))))
    (list "-bg"
	  (concat "rgb "
		  (mapconcat
		   (lambda (col)
		     (let ((val (/ (float col) max)))
		       (cond ((> val 1.0) (setq val 1.0))
			     ((< val 0.0) (setq val 0.0)))
		       (format "%.02f" val)))
		   (color-values (or latex-math-preview-image-background-color
				     (face-background 'default))) " "))
	  "-fg"
	  (concat "rgb "
		  (mapconcat
		   (lambda (col)
		     (let ((val (/ (float col) max)))
		       (cond ((> val 1.0) (setq val 1.0))
			     ((< val 0.0) (setq val 0.0)))
		       (format "%.02f" val)))
		   (color-values (or latex-math-preview-image-foreground-color
				     (face-foreground 'default))) " ")))))

(defun latex-math-preview-dvi-to-png (filename &optional output)
  "Render dvi FILENAME to an Emacs image and return that.
The \"dvipng\" program is used for drawing.  If it fails a shell
buffer is left showing the messages and the return is nil."

  (if (not latex-math-preview-dvipng-color-option)
      (setq latex-math-preview-dvipng-color-option (latex-math-preview-get-dvipng-color-option)))
  (let ((dot-png (or output (concat latex-math-dir "/"
				    latex-math-preview-temporary-file-prefix ".png"))))
    (if (eq 0 (eval `(call-process latex-math-preview-command-dvipng nil
				   latex-math-preview-dvipng-log-buffer nil "-o" dot-png
				   ,@latex-math-preview-dvipng-option
				   ,@latex-math-preview-dvipng-color-option filename)))
	dot-png
      nil)))

;;-----------------------------------------------------------------------------
;; Manage window

(defun latex-math-preview-quit-window ()
  "Quit preview window."
  (interactive)
  (set-window-configuration latex-math-preview-window-configuration))

(defun latex-math-preview-delete-buffer ()
  "Delete buffer which is created for preview."
  (interactive)
  (kill-buffer (buffer-name))
  (set-window-configuration latex-math-preview-window-configuration))

; Clear dvipng option for coloring.
(setq latex-math-preview-dvipng-color-option nil)

;;-----------------------------------------------------------------------------
;; Insert Mathematical expression 

(defun latex-math-preview-clear-cache-for-insertion (&optional dirname)
  "Delete cache images in DIRNAME.
If DIRNAME is nil then all directories saving caches is deleted."
  (interactive)
  (if dirname
      (if (assoc dirname latex-math-preview-candidates-for-insertion)
	  (latex-math-preview-clear-tmp-directory
	   (concat latex-math-preview-cache-directory-for-insertion "/" dirname)))
    (latex-math-preview-clear-tmp-directory latex-math-preview-cache-directory-for-insertion)))

(defun latex-math-preview-make-candidate-image (math-symbol dirname num)
  "Create a cache image from latex file for including MATH-SYMBOL.
Image is saved in DIRNAME. NUM is used for distingushing other images."
  (let ((latex-math-dir (make-temp-file "latex-math-preview-" t))
	(path (concat latex-math-preview-cache-directory-for-insertion "/" dirname "/"
		      (format "%05d" num) "_"
		      (downcase (replace-regexp-in-string "\\(\\\\\\)\\|\\({\\)\\|\\(}\\)"
							  "_" math-symbol)) ".png")))
    (latex-math-preview-dvi-to-png
     (latex-math-preview-make-dvi-file latex-math-dir (concat "$" math-symbol "$")) path)
    (latex-math-preview-clear-tmp-directory latex-math-dir)
    path))

(defun latex-math-preview-make-cache-for-insertion (dirname)
  "Create cache images in DIRNAME."
  (let ((latex-syms (cdr (assoc dirname latex-math-preview-candidates-for-insertion)))
	(dirpath (concat latex-math-preview-cache-directory-for-insertion "/" dirname))
	(num 0))
    (if (file-directory-p dirpath)
	(message "'%s' exists. Cache may be used." dirpath)
      (progn
	(make-directory dirpath t)
	(message "Creating images. Please wait for a while.")
	(dolist (sym latex-syms)
	  (if (listp sym)
	      (latex-math-preview-make-candidate-image (eval `(concat ,@sym)) dirname num)
	    (latex-math-preview-make-candidate-image sym dirname num))
	  (setq num (+ num 1)))))))

(defun latex-math-preview-make-all-cache-images ()
  "Create all cache images."
  (interactive)
  (dolist (data latex-math-preview-candidates-for-insertion)
    (latex-math-preview-make-cache-for-insertion (car data))))

(defun latex-math-preview-strings-and-images-sizes (images latex-syms)
  "Look over cache images.
Return maximum size of images and maximum length of strings and images"
  (setq latex-math-preview-candidates-defined-as-list nil)
  (let ((max-img-size 0)
	(max-str-length 0)
	(img-list nil)
	(syms latex-syms)
	(ret-list nil))
    (dolist (pngpath images)
      (let* ((img (create-image pngpath	'png nil :ascent 'center))
	     (size (car (image-size img t))))
	(if (< max-img-size size) (setq max-img-size size))
	(let ((s (car syms)) (str))
	  (if (listp s)
	      (progn
		(setq str (eval `(concat ,@s)))
		(add-to-list 'latex-math-preview-candidates-defined-as-list `(,str ,s)))
	    (setq str s))
	  (let ((len (length str)))
	    (if (< max-str-length len) (setq max-str-length len)))
	  (add-to-list 'img-list `(,str ,img)))
	(setq syms (cdr syms))))
    `(,max-img-size ,max-str-length ,(nreverse img-list))))

(defun latex-math-preview-create-buffer-for-insertion (dirname)
  "Create buffer displaying cache images in DIRNAME."
  (or (and (image-type-available-p 'png)
           (display-images-p))
      (error "Cannot display PNG in this Emacs"))

  (setq latex-math-preview-current-group dirname)
  (latex-math-preview-make-cache-for-insertion dirname)
  (setq latex-math-preview-window-configuration (current-window-configuration))
  (with-current-buffer (get-buffer-create latex-math-preview-insert-symbol-buffer-name)
    (setq cursor-type nil)
    (setq truncate-lines t)
    (setq line-spacing 8)
    (setq buffer-read-only nil)
    (erase-buffer)

    (insert "key:")
    (add-text-properties (point-min) (point) '(face bold))
    (insert " [RET] insert   [j] down   [k] up   [h] left   [l] right")
    (insert "\n")
    (insert "     [.] next page   [,] previous page   [c] change group   [q] quit")
    (insert "\n")
    
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[" nil t)
	(backward-char)
	(let ((start-pt (point)))
	  (if (re-search-forward "\\]" nil t)
	      (add-text-properties start-pt (point)
				   '(face latex-math-preview-key-for-insertion-face))))))

    (let ((num-dash (- (/ (- (frame-width) (length dirname)) 2) 3))
	  (start-pt))
      (insert (make-string num-dash ?-))
      (setq start-pt (point))
      (insert " * " dirname " * ")
      (add-text-properties start-pt (point) '(face bold))
      (insert (make-string num-dash ?-)))
    (insert "\n")
    (setq latex-math-preview-number-start-candidates 4)

    (let* ((latex-symbols (cdr (assoc dirname latex-math-preview-candidates-for-insertion)))
	   (dirpath (concat latex-math-preview-cache-directory-for-insertion "/" dirname))
	   (data (latex-math-preview-strings-and-images-sizes
		  (mapcar (lambda (path) (concat latex-math-preview-cache-directory-for-insertion
						 "/" dirname "/" path))
			   (sort (delete-if (lambda (file) (not (string-match "^[0-9]+_" file)))
					    (directory-files dirpath)) 'string<)) latex-symbols))
	   (new-tab-width (+ 4 (ceiling (/ (float (car data)) (float (frame-char-width))))))
	   (str-size (* new-tab-width (ceiling (/ (float (+ 6 (car (cdr data)))) (float new-tab-width)))))
	   ;; You must not remove (+ 6 ...).
	   ;; Implementation of latex-math-preview-set-overlay-for-selected-item needs redundant space.
	   (str-format (format "%%-%ds" str-size))
	   (row (floor (/ (frame-width)
			  (+ str-size (* (ceiling (/ (float (car data))
						     (float (* (frame-char-width) new-tab-width))))
					 new-tab-width)))))
	   (num row))

      (setq tab-width new-tab-width)
      (dolist (imgdata (car (cdr (cdr data))))
	  (insert-image (car (cdr imgdata)))
	  (insert "\t")
	  (let ((start-pt (point)))
	    (insert (format str-format (car imgdata)))
	    (add-text-properties start-pt (point) '(face latex-math-preview-candidate-for-insertion-face)))
      	  (setq num (- num 1))
      	  (if (<= num 0)
      	      (progn
      		(setq num row)
      		(insert "\n")))))

    (goto-line latex-math-preview-number-start-candidates)
    (latex-math-preview-move-to-right-item)
    (buffer-disable-undo)
    (setq buffer-read-only t)
    (use-local-map latex-math-preview-insert-symbol-map)
    (setq mode-name "LaTeXPreview"))
  (pop-to-buffer latex-math-preview-insert-symbol-buffer-name))

(defun latex-math-preview-insert-symbol (&optional num)
  "Insert LaTeX mathematical symbols with displaying."
  (interactive "p")
  (if (= num 1)
      (latex-math-preview-create-buffer-for-insertion
       (or latex-math-preview-current-group latex-math-preview-initial-group))
    (let ((dirname (completing-read "group: " latex-math-preview-candidates-for-insertion nil t)))
      (latex-math-preview-create-buffer-for-insertion dirname))))

(defun latex-math-preview-symbols-of-other-group ()
  "Change other page."
  (interactive)
  (latex-math-preview-quit-window)
  (latex-math-preview-insert-symbol -1))

(defun latex-math-preview-get-page-number (dirname)
  "Get number of page for DIRNAME."
  (let ((num 0) (cont t)
	(max-num (length latex-math-preview-candidates-for-insertion)))
    (while (and cont (< num max-num))
      (if (string= dirname (car (nth num latex-math-preview-candidates-for-insertion)))
	  (setq cont nil))
      (setq num (1+ num)))
    (if (not cont)
	(- num 1)
      nil)))

(defun latex-math-preview-next-candidates-for-insertion (num)
  "Next page of candidates buffer for insertion."
  (interactive "p")
  (let ((page (latex-math-preview-get-page-number latex-math-preview-current-group))
	(len (length latex-math-preview-candidates-for-insertion)))
    (while (< num 0) (setq num (+ num len)))
    (if page
	(let ((dirname (car (nth (% (+ page num) len)
				 latex-math-preview-candidates-for-insertion))))
	  (latex-math-preview-quit-window)
	  (latex-math-preview-create-buffer-for-insertion dirname)))))

(defun latex-math-preview-previous-candidates-for-insertion (num)
  "Previous page of candidates buffer for insertion."
  (interactive "p")
  (latex-math-preview-next-candidates-for-insertion (- 0 num)))

(defun latex-math-preview-symbol-insertion (str)
  "Execute insertion from STR."
  (let ((sym (assoc str latex-math-preview-candidates-defined-as-list)))
    (if sym
	(progn
	  (insert (car (car (cdr sym))))
	  (save-excursion
	    (insert (car (cdr (cdr (car (cdr sym))))))))
      (insert str))))

(defun latex-math-preview-put-selected-candidate ()
  "Insert selected LaTeX mathematical symboled to original buffer."
  (interactive)
  (let* ((str (buffer-substring
	       (overlay-start latex-math-preview-selection-overlay-for-insertion)
	       (overlay-end latex-math-preview-selection-overlay-for-insertion))))
    (latex-math-preview-quit-window)
    (latex-math-preview-symbol-insertion str)
    (setq latex-math-preview-inserted-last-symbol str)))

(defun latex-math-preview-last-symbol-again ()
  "Insert last symbol which is inserted by `latex-math-preview-insert-symbol'"
  (interactive)
  (if latex-math-preview-inserted-last-symbol
      (latex-math-preview-symbol-insertion latex-math-preview-inserted-last-symbol)))

;;-----------------------------------------------------------------------------
;; Move to other item

(defun latex-math-preview-set-overlay-for-selected-item ()
  "Set overlay and highlight."
  (save-excursion
    (let ((start-ol))
      (skip-chars-backward "^\t")
      (setq start-ol (point))
      (re-search-forward "  " nil t)
      (skip-chars-backward " ")
      (if latex-math-preview-selection-overlay-for-insertion
	  (move-overlay latex-math-preview-selection-overlay-for-insertion start-ol (point))
	(progn
	  (setq latex-math-preview-selection-overlay-for-insertion (make-overlay start-ol (point)))
	  (overlay-put latex-math-preview-selection-overlay-for-insertion 'face latex-math-preview-selection-face-for-insertion))))))

(defun latex-math-preview-move-to-right-item ()
  "Move to right item."
  (interactive)
  (re-search-forward "\t\\([^\t]+\\)  " nil t)
  (goto-char (match-beginning 1))
  (latex-math-preview-set-overlay-for-selected-item))

(defun latex-math-preview-move-to-left-item ()
  "Move to left item."
  (interactive)
  (re-search-backward "\t\\([^\t]+\\)  " nil t)
  (goto-char (match-beginning 1))
  (latex-math-preview-set-overlay-for-selected-item))

(defun latex-math-preview-move-to-upward-item ()
  "Move to upward item."
  (interactive)
  (if (< latex-math-preview-number-start-candidates (line-number-at-pos))
      (progn
	(let ((col (current-column)))
	  (forward-line -1)
	  (move-to-column col))
	(skip-chars-backward "^\t ")
	(backward-char)
	(latex-math-preview-move-to-right-item))))

(defun latex-math-preview-move-to-downward-item ()
  "Move to downward item."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (move-to-column col))
  (skip-chars-backward "^\t ")
  (backward-char)
  (latex-math-preview-move-to-right-item))

(provide 'latex-math-preview)

;;; latex-math-preview.el ends here
