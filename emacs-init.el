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
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(safe-local-variable-values (quote ((org-export-latex-image-default-option . "width=30em") (org-export-latex-image-default-option . "width=100em") (org-babel-default-header-args (:tangle . "wtccc2-pca.py")) (org-babel-default-header-args (:tangle . "wtccc2-pca.py") (:exports . "code")) (org-babel-default-header-args (:results . "replace output") (:session . "*R - jsmr*") (:exports . "none")) (org-babel-default-header-args (:results . "replace output") (:session . "*R: wtccc2*") (:exports . "none")) (noweb-default-code-mode . R-mode) (org-src-preserve-indentation . t) (org-edit-src-content-indentation . 0) (outline-minor-mode)))))
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
;;  '(gnus-cite-1 ((((class color) (background light)) (:foreground "deep sky blue")))))
