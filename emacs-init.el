(setq max-mini-window-height nil)
(add-to-list 'load-path "~/lib/emacs/org/lisp")
(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)

(org-babel-load-file "~/config/emacs/emacs.org")
(load-file "~/config/emacs/extra.el")
