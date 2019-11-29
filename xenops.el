(dan/alist-update!
 xenops-text-prettify-symbols
 '(("\\R" . "ℝ")
   ("\\N" . "ℕ")
   ("\\C" . "ℂ")
   ("\\Q" . "ℚ")
   ("\\Z" . "ℤ")
   ("\\Lag" . "L")

   ;; ("\\\\" . "⏎")
   ("\\\\" . "")

   ("\\(" . "(")
   ("\\)" . ")")
   ("``" . "\"")
   ("''" . "\"")
   (" ~ " . " ")))

(dan/alist-update!
 xenops-text-prettify-symbols-string-replacements
 '(("\\begin{question*}" . "Question.")
   ("\\end{question*}" . "┘")

   ("\\begin{example*}" . "Example.")
   ("\\end{example*}" . "┘")

   ("\\begin{example}" . "Example.")
   ("\\end{example}" . "┘")

   ("\\begin{claim*}" . "Claim.")
   ("\\end{claim*}" . "┘")

   ("\\begin{intuition*}" . "Intuition.")
   ("\\end{intuition*}" . "┘")

   ("\\begin{intuition*}" . "Intuition.")
   ("\\end{intuition*}" . "┘")

   ("\\begin{intuition}" . "Intuition.")
   ("\\end{intuition}" . "┘")

   ("\\begin{minted}" . "⚡")
   ("\\end{minted}" . "⚡")

   ("#+begin_src" . "⚡")
   ("#+end_src" . "⚡")

   ("\\begin{comment}  % latex-focus" .
    "\\begin{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
   ("\\end{comment}  % latex-focus" .
    "\\end{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

   ("\\begin{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
   ("\\end{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

   ;; ("\\newpage" .
   ;;  "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

   ("\\begin{enumerate}[label=(\\alph*)]" . "┐")

   ("\\correct" . "☑")
   ("\\todo" . "TODO")

   ("\\vecMMM" . "\\vec")
   ("\\bvecMMM" . "\\vec")

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


   ;; ("$" . " " ) ;; ⚡  "​" zero-width space
   ("\\d\\r" . "dr")
   ;; TODO: dangerous?, will this clash with anything starting with \r?
   ("\\r" . "r")
   ("\\v" . "v")
   ("\\F" . "F")))
