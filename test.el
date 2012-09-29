(require 'ert)
(require 'latex-math-preview)

(ert-deftest test-get-headers ()
  (let ((packages)
	(latex-math-preview-usepackage-filter-alist (list '("graphicx"))))
    (with-temp-buffer
      (insert-file "test.tex")
      (setq packages (latex-math-preview-get-header-usepackage)))
    (should (equal '("\\usepackage{amsmath, amsfonts, amssymb, amsthm}"
		     "\\usepackage[all]{xy}"
		      "\\DeclareMathOperator{\\image}{\\mathrm{Im}}"
		      "\\providecommand{\\abs}[1]{\\lvert#1\\rvert}") packages))))

(ert-run-tests-batch-and-exit)
