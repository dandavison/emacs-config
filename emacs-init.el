(setq max-mini-window-height nil)
(add-to-list 'load-path "/usr/local/src/emacs/org-mode/lisp")
(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(org-babel-load-file "~/config/emacs/emacs.org")

;; (add-to-list 'load-path "~/emacs/emacs-starter-kit")
;; (load-file "~/emacs/emacs-starter-kit/init.el")
;; (org-babel-load-file "~/emacs/emacs-starter-kit/starter-kit.org")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "c6d84111c8952c110a560f52e80fdd8dc62bcf1d")
 '(safe-local-variable-values (quote ((varname . value) (noweb-default-code-mode . R-mode) (outline-minor-mode)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
