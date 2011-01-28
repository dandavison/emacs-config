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
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "596bc0dc6e2f3391e0f1a381451ca8d297f9abf8")
 '(safe-local-variable-values (quote ((eval sbe "document-config")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-meta-line ((t (:inherit font-lock-comment-face :foreground "Purple")))))
