(add-to-list 'load-path "/usr/local/src/org-mode/lisp")
(add-to-list 'load-path "~/src/org-babel/lisp")
(require 'org-install)
(require 'org-babel-init)
(org-babel-load-file "~/config/emacs/emacs.org")

;;*
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((org-babel-default-header-args (:results . "output") (:session . "*R*")) (noweb-default-code-mode . R-mode) (outline-minor-mode)))))
(if nil
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
