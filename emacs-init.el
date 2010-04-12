(add-to-list 'load-path "/usr/local/src/emacs/org-mode/lisp")
(add-to-list 'load-path "/usr/local/src/emacs/org-mode/contrib/lisp")
(require 'org-install)
(require 'org-babel-init)
(setq dan/org-babel-languages
      '(R python ruby ditaa latex asymptote dot sass perl sql gnuplot clojure octave matlab C))
(setq dan/org-babel-early-load-languages
      '(R))
(mapc '(lambda (lang) (require (intern (format "org-babel-%s" lang))))
      dan/org-babel-early-load-languages)
(org-babel-load-file "~/config/emacs/emacs.org")
