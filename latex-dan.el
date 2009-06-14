;; (load "auctex")
(defun latex-mode-hook-dan ()
  ;; (setq TeX-auto-save t)   what
  ;; (setq TeX-parse-self t)     are
  ;; (setq-default TeX-master nil)   these?
  (setq TeX-electric-sub-and-superscript t)
  ;; (TeX-fold-mode 1)
  (reftex-mode)
  (load "~/src/config/emacs/latex-reftex-toc-altered")
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	dan-latex-mode-keybindings)
  (mapc (lambda(pair) (global-set-key (car pair) (cdr pair)))
      dan-global-keybindings)
  (setq case-fold-search nil) ;; dunno why it wouldn't be but it wasn't
  )

(add-hook 'latex-mode-hook 'latex-mode-hook-dan)

;; these not having desired effect yet
;; (setq reftex-toc-keep-other-windows nil)
;; (add-hook 'reftex-toc-post-command-hook (lambda () (delete-other-windows)))
;; how about



