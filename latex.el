(setq preview-image-type 'dvipng)

(defun dan/latex-toggle-TeX-electric-math (&optional arg)
  (interactive "P")
  (setq TeX-electric-math
        (if TeX-electric-math
            nil
          (if arg '("\\(" . "\\)") '("$" . "$"))))
  (message "%S" TeX-electric-math))

(defun dan/latex-mode-hook-fn ()
  (interactive)

  ;; ;; Auctex
  ;; ;; (setq TeX-auto-save t)
  ;; ;; (setq TeX-parse-self t)
  ;; ;; (setq-default TeX-master nil)
  (setq blink-matching-delay 0.2
        LaTeX-electric-left-right-brace t
        LaTeX-command "lualatex -shell-escape"
        TeX-engine 'luatex
        TeX-electric-sub-and-superscript nil)
  ;; ;; (setq preview-image-type 'dvipng
  ;; ;;       preview-dvipng-image-type 'svg
  ;; ;;       TeX-PDF-from-DVI "Dvips")

  (unless TeX-electric-math (dan/latex-toggle-TeX-electric-math))
  (outline-minor-mode)
  (prettify-symbols-mode)


  ;; (dan/set-up-outline-minor-mode "\\(\\\\sub\\|\\\\section\\|\\\\begin\\|\\\\item\\)")

  ;; (add-to-list 'LaTeX-item-list
  ;;              '("align" . dan/latex-insert-item-in-align-environment))
  ;; (add-to-list 'LaTeX-item-list
  ;;              '("align*" . dan/latex-insert-item-in-align-environment))

  (setq fill-column 111
        fci-rule-column fill-column
        tab-width 2)

  ;; (setq-local indent-line-function 'dan/latex-indent-line-function)
  ;; (setq LaTeX-indent-environment-list (cons '("minted" . nil) LaTeX-indent-environment-list))
  ;; (when nil
  ;;   (add-hook 'before-save-hook
  ;;             (lambda () (unless (string-match ".+\\.sty" (buffer-file-name)) (dan/indent-buffer)))
  ;;             nil 'local))
  (when t (xenops-mode)))
