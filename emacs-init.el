(add-to-list 'load-path "/usr/local/src/emacs/org-mode/lisp")
(require 'org-install)
(setq dan/org-babel-languages
      '(R python sh ruby ditaa latex asymptote dot sass perl sql gnuplot clojure octave matlab C))
(setq dan/org-babel-early-load-languages
      '(R))
(mapc '(lambda (lang) (require (intern (format "ob-%s" lang))))
      dan/org-babel-early-load-languages)
(setq max-mini-window-height nil)
(org-babel-load-file "~/config/emacs/emacs.org")

;; (add-to-list 'load-path "~/emacs/emacs-starter-kit")
;; (load-file "~/emacs/emacs-starter-kit/init.el")
;; (org-babel-load-file "~/emacs/emacs-starter-kit/starter-kit.org")
