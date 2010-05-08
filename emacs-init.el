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
(setq max-mini-window-height nil)
(org-babel-load-file "~/config/emacs/emacs.org")

;; (add-to-list 'load-path "~/emacs/emacs-starter-kit")
;; (load-file "~/emacs/emacs-starter-kit/init.el")
;; (org-babel-load-file "~/emacs/emacs-starter-kit/starter-kit.org")
