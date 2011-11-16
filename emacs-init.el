(setq max-mini-window-height nil)
(add-to-list 'load-path "~/lib/emacs/org/lisp")
(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
(require 'org-install)
(require 'org)

(org-babel-load-file "~/config/emacs/emacs.org")
(load-file "~/config/emacs/extra.el")

;; ;; (add-to-list 'load-path "~/emacs/emacs-starter-kit")
;; ;; (load-file "~/emacs/emacs-starter-kit/init.el")
;; ;; (org-babel-load-file "~/emacs/emacs-starter-kit/starter-kit.org")
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(canlock-password "c0f183afebc9e2c0b20627e301c66409604d7c1d")
;;  '(csv-separators (quote (",")))
;;  '(safe-local-variable-values (quote ((eval sbe "document-config") (noweb-default-code-mode . R-mode) (outline-minor-mode))))
;;  '(session-use-package t nil (session)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "#232323" :foreground "#E6E1DC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Monaco"))))
;;  '(magit-section-title ((t nil))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "6be815a8b1ebd5880b78a2c06ecfb1b908c8ec74")
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-input ((t (:weight normal)))))
