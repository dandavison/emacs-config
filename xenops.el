(setq org-format-latex-header "\\documentclass[final, conference]{IEEEtran}
\\usepackage[usenames]{color}
[PACKAGES]
[DEFAULT-PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}")

;; (setq xenops-font-font-family "Latin Modern Math")
(setq xenops-xen-style-rules
      (append xenops-xen-style-rules
              '(("\\R" . "ℝ")
                ("\\N" . "ℕ")
                ("\\C" . "ℂ")
                ("\\Q" . "ℚ")
                ("\\Z" . "ℤ")
                ("\\Lag" . "L")

                ;; ("\\\\" . "⏎")
                ("\\\\" . " ")

                (" ~ " . " ")

                ("\\correct" . "☑")
                ("\\todo" . "TODO")

                ("\\vecMMM" . "\\vec")
                ("\\bvecMMM" . "\\vec")

                ("\\begin{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
                ("\\end{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

                ;; TODO: DNW?
                ("\\Bigg[" . "[")
                ("\\Bigg]" . "]")
                ("\\Bigg(" . "(")
                ("\\Bigg)" . ")")

                ("&=" . "=")

                ("\\dt" . "dt")
                ("\\du" . "du")
                ("\\dv" . "dv")
                ("\\dx" . "dx")
                ("\\dy" . "dy")

                ;; https://unicode-table.com/en/0307/
                ;; U+0307 Combining Dot Above
                ("\\dot{\\r}" . "ṙ")
                ("\\dot{\\v}" . "v̇")
                ("\\dot{x}" . "ẋ")
                ("\\dot{y}" . "ẏ")

                ("\\xdot" . "ẋ")
                ("\\ydot" . "ẏ")

                ("\\ddot{\\r}" . "r̈")

                ("\\xddot" . "ẍ")
                ("\\yddot" . "ÿ")

                ("\\infty" . "∞")
                ("\\to" . "⟶")

                ;; ("$" . " " ) ;; ⚡  "​" zero-width space
                ("\\d\\r" . "dr")
                ("\\r" . "r")
                ("\\v" . "v")
                ("\\F" . "F"))))
