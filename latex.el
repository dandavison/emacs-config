(setq preview-image-type 'dvipng)

(defun dan/latex-mode-hook-fn ()
  (interactive)

  ;; Auctex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  ;; (setq preview-image-type 'dvipng
  ;;       preview-dvipng-image-type 'svg
  ;;       TeX-PDF-from-DVI "Dvips")

  (setq org-latex-packages-alist nil
        org-latex-default-packages-alist nil)

  (dan/latex-paired-characters)

  (dan/set-up-outline-minor-mode "\\(\\\\sub\\|\\\\section\\|\\\\begin\\|\\\\item\\)")

  ;; (dan/watch-mathematics)
  (add-to-list 'LaTeX-item-list
               '("align" . dan/latex-insert-item-in-align-environment))
  (add-to-list 'LaTeX-item-list
               '("align*" . dan/latex-insert-item-in-align-environment))

  (setq xenops-image-latex-template
        "\\begin{mdframed}\n\\includegraphics[width=400pt]{%s}\n\\end{mdframed}")

  (setq fill-column 150
        fci-rule-column fill-column
        tab-width 2)
  (setq-local indent-line-function 'dan/latex-indent-line-function)
  (setq LaTeX-indent-environment-list (cons '("minted" . nil) LaTeX-indent-environment-list))
  (when nil
    (add-hook 'before-save-hook
              (lambda () (unless (string-match ".+\\.sty" (buffer-file-name)) (dan/indent-buffer)))
              nil 'local))
  (xenops-mode))
