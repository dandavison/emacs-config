(setq max-mini-window-height nil)
(add-to-list 'load-path "~/lib/emacs/org/lisp")
(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)
(org-babel-load-file "~/config/emacs/emacs.org")

;; (add-to-list 'load-path "~/emacs/emacs-starter-kit")
;; (load-file "~/emacs/emacs-starter-kit/init.el")
;; (org-babel-load-file "~/emacs/emacs-starter-kit/starter-kit.org")
(custom-set-variables
 '(safe-local-variable-values (quote ((eval sbe "document-config")))))
