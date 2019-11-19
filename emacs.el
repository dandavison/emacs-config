;;; Init
(setq debug-on-error nil)
(unless (equal emacs-version "27.0.50") (package-initialize))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq use-package-always-demand t)

;;; MacOS
;; https://github.com/railwaycat/homebrew-emacsmacport/
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;;; Native packages

(use-package emacs
  :bind (("C-M-\\" . dan/indent-region)
         ("C-b" . backward-sexp)
         ("C-c 1" . flycheck-mode)
         ("C-c C-c" . dan/save-even-if-not-modified)
         ("C-c C-l" . (lambda () (interactive) (eval-buffer) (message "eval-buffer: %s" (buffer-file-name))))
         ("C-c C-r" . magit-file-rename)
         ("C-c C-z" . python-shell-switch-to-shell)
         ("C-c M-d" . dan/magit-diff)
         ("C-c M-f" . search-files-thing-at-point)
         ("C-c b" . magit-blame)
         ("C-c c" . (lambda () (interactive) (magit-show-commit "HEAD")))
         ("C-c d" . (lambda (&optional arg) (interactive "P") (setq debug-on-error (not arg))))
         ("C-c e" . outline-show-all)
         ("C-c f" . search-files-by-name)
         ("C-c g" . magit-status)
         ("C-c l" . linum-mode)
         ("C-c m" . dan/goto-messages-buffer)
         ("C-c o" . dan/scratch-buffer)
         ("C-p" . prettify-symbols-mode)
         ("C-c r" . replace-regexp)
         ("C-c s" . (lambda () (interactive) (shell-command-on-region (region-beginning) (region-end) "sort -V" nil 'replace)))
         ("C-c w" . dan/list-window-configurations)
         ("C-f" . forward-sexp)
         ("C-g" . dan/keyboard-quit)
         ("C-s" . swiper)
         ("C-x C-f" . dan/find-file)
         ("C-x b" . dan/switch-to-buffer)
         ("C-x d" . dan/dired-no-ask)
         ("C-x C-l" . find-library)
         ("C-x n n" . dan/narrow-to-region)
         ("C-x n w" . dan/widen)
         ("C-x p" . projectile-switch-project)
         ("M-i" . dan/highlight)
         ("M-o" . swiper-thing-at-point)
         ("M-q" . fill-paragraph)
         ([f1] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?1 arg)))
         ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
         ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
         ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
         ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
         ([f6] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?6 arg)))
         ([f7] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?7 arg)))
         ([f8] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?8 arg)))
         ([f9] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?9 arg)))
         ([f10] . (lambda () (interactive) (find-file "~/dandavison7@gmail.com/Projects/xenops.org")))
         ([f11] . dan/goto-emacs-config)
         ([f12] . dan/goto-erc-emacs)
         ([(super down)] . avy-goto-line)
         ([(super up)] . avy-goto-line)
         ([(kp-delete)] . modalka-mode)
         ([(meta down)] . dan/transpose-line-down)
         ([(meta left)] . backward-word)
         ([(meta right)] . forward-word)
         ([(meta shift left)] . dan/indent-shift-left)
         ([(meta shift right)] . dan/indent-shift-right)
         ([(meta up)] . dan/transpose-line-up)
         ([(meta ?,)] . ace-jump-mode)
         ([(meta ?.)] . dan/goto-definition)
         ([(shift super left)] . ivy-resume)
         ([(super G)] . isearch-repeat-backward)
         ([(super b)] . dan/switch-to-buffer)
         ([(super d)] . dan/bookmark-set)
         ([(super f)] . dan/find-file)
         ([(super k)] . dan/bookmark-set)
         ([(super left)] . winner-undo)
         ([(super l)] . bookmark-bmenu-list)
         ([(super m)] . dan/goto-definition)
         ([(super n)] . (lambda () (interactive) (dan/switch-to-buffer '(4))))
         ([(super o)] . dan/find-file)
         ([(super return)] . dan/maximize)
         ([(super right)] . winner-redo)
         ([(super u)] . revert-buffer) ;; not bound in MacOS port
         ([(super v)] . yank) ;; not bound in MacOS port
         ([(super ?&)] . (lambda () (interactive) (let ((kill-buffer-query-functions nil)) (kill-buffer))))
         ([(super ?,)] . counsel-rg)
         ([(super ?.)] . dan/grep-thing-at-point)
         ([(super ?\;)] . dan/show-buffer-file-name)
         ([(super ?\])] . fci-mode)
         ([(super \\)] . dan/indent-region)
         ([(super |)] . dan/shell-command-on-region-and-replace)))

(use-package bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("C-x C-s" . bookmark-save)))

(use-package cl)

(use-package comint
  :bind (:map comint-mode-map
              ([(meta up)] . comint-previous-matching-input-from-input)
              ([(meta down)] . comint-next-matching-input-from-input)
              ([(control up)] . comint-previous-matching-input-from-input)
              ([(control down)] . comint-next-matching-input-from-input)
              ("C-l" . dan/comint-clear-buffer)))

(use-package compilation
  :defer t
  :bind (:map compilation-mode-map
              ([(return)] . compile-goto-error)
              ("C-c d" . dan/delete-matching-lines)
              ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (dan/iterm2-dwim))))
  :config
  (setq compilation-ask-about-save nil
        compilation-save-buffers-predicate (lambda () nil)))

(use-package dired
  :bind (:map dired-mode-map
              ([left] . dired-up-directory)
              ([right] . dired-find-file))
  :config
  (setq dired-auto-revert-buffer t
        dired-omit-size-limit nil
        dired-omit-files "^__pycache__\\|^.mypy_cache\\|^\\.\\.?\\|\\.pyc")
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-omit-files-p t))

(use-package dired-x)

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("C-c d" . edebug-defun)
              ("C-c ," . find-function)
              ("C-c C-r" . (lambda () (interactive) (call-interactively 'eval-region) (deactivate-mark)))
              ([(super x)] . eval-defun)
              ([(meta left)] . nil)
              ([(meta right)] . nil))
  :hook (emacs-lisp-mode . (lambda () (flycheck-mode -1))))

(use-package erc
  :config
  (setq erc-nick "dd7"
        erc-hide-list '("JOIN" "PART" "QUIT")))

(use-package org
  :load-path "~/src/3p/org-mode/lisp"
  :after org-table
  :bind (:map org-mode-map
              ([(shift left)] . windmove-left)
              ([(shift right)] . windmove-right)
              ([(shift up)] . windmove-up)
              ([(shift down)] . windmove-down)
              ([(meta left)] . nil)
              ([(meta right)] . nil)
              :map orgtbl-mode-map
              ([(meta left)] . nil)
              ([(meta right)] . nil))

  :hook
  (org-mode . (lambda ()
                (local-set-key [(meta left)] 'backward-word)
                (local-set-key [(meta right)] 'forward-word)
                (local-set-key [(shift left)] 'windmove-left)
                (local-set-key [(shift right)] 'windmove-right)))
  :config
  (setq org-support-shift-select 'always))

(use-package outline
  :bind (:map outline-minor-mode-map
              ([(control tab)] . org-cycle)
              ([(backtab)] . org-global-cycle)))

(use-package python
  :bind (:map python-mode-map
              ("C-c d" . dan/python-insert-ipdb-set-trace)
              ("C-c C-c" . dan/save-even-if-not-modified)
              ([(super ?b)] . dan/blacken-region)
              ([(super ?')] . flycheck-mode)
              ([(super i)] . dan/python-where-am-i)
              ([(meta shift right)] . python-indent-shift-right)
              ([(meta shift left)] . python-indent-shift-left)
              ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (jedi:goto-definition))))
  :hook (python-mode . blacken-on-save-mode))


(load-file "~/src/emacs-config/lib.el")


;;; External Packages

(use-package ace-window
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package clojure-mode
  :defer t
  :bind (:map clojure-mode-map
              ("C-x C-e" . inf-clojure-eval-last-sexp)
              ("C-c C-z" . inf-clojure)))

(use-package company
  :bind (:map prog-mode-map
              ([tab] . dan/company-indent-or-complete))
  :hook (prog-mode . company-mode)
  :config
  (setq company-selection-wrap-around t))

(use-package company-jedi
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi))

(use-package dash)

(use-package xenops
  :load-path "~/src/xenops"
  :bind (:map xenops-mode-map
              ("C-p" . xenops-text-prettify-symbols-mode)
              ("C-x c" . xenops-avy-copy-math-and-paste)
              ("C-x x" . xenops-org-babel-execute-src-block))
  :config
  (dan/alist-update!
   xenops-text-prettify-symbols
   '(("\\R" . "ℝ")
     ("\\N" . "ℕ")
     ("\\C" . "ℂ")
     ("\\Q" . "ℚ")
     ("\\Lag" . "L")

     ;; ("\\\\" . "⏎")
     ("\\\\" . "")

     ("\\(" . "(")
     ("\\)" . ")")
     ("``" . "\"")
     ("''" . "\"")
     (" ~ " . " ")))
  (dan/alist-update!
   xenops-text-prettify-symbols-string-replacements
   '(("\\begin{question*}" . "Question.")
     ("\\end{question*}" . "┘")

     ("\\begin{example*}" . "Example.")
     ("\\end{example*}" . "┘")

     ("\\begin{example}" . "Example.")
     ("\\end{example}" . "┘")

     ("\\begin{claim*}" . "Claim.")
     ("\\end{claim*}" . "┘")

     ("\\begin{intuition*}" . "Intuition.")
     ("\\end{intuition*}" . "┘")

     ("\\begin{intuition*}" . "Intuition.")
     ("\\end{intuition*}" . "┘")

     ("\\begin{intuition}" . "Intuition.")
     ("\\end{intuition}" . "┘")

     ("\\begin{comment}  % latex-focus" .
      "\\begin{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
     ("\\end{comment}  % latex-focus" .
      "\\end{comment}  % latex-focus ⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

     ("\\begin{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")
     ("\\end{mdframed}" . "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯")

     ("#+begin_src" . "⚡")
     ("#+end_src" . "⚡")

     ("\\begin{enumerate}[label=(\\alph*)]" . "┐")

     ("\\correct" . "☑")
     ("\\todo" . "TODO")

     ("\\vecMMM" . "\\vec")
     ("\\bvecMMM" . "\\vec")

     ;; TODO: DNW?
     ("\\Bigg[" . "[")
     ("\\Bigg]" . "]")
     ("\\Bigg(" . "(")
     ("\\Bigg)" . ")")

     ("&=" . "=")

     ("\\dt" . "dt")
     ("\\du" . "du")
     ("\\dv" . "dv")
     ("\\dx" . "dx")
     ("\\dy" . "dy")

     ;; https://unicode-table.com/en/0307/
     ;; U+0307 Combining Dot Above
     ("\\dot{\\r}" . "ṙ")
     ("\\dot{\\v}" . "v̇")
     ("\\dot{x}" . "ẋ")
     ("\\dot{y}" . "ẏ")

     ("\\xdot" . "ẋ")
     ("\\ydot" . "ẏ")

     ("\\ddot{\\r}" . "r̈")

     ("\\xddot" . "ẍ")
     ("\\yddot" . "ÿ")


     ("$" . " " ) ;; ⚡  "​" zero-width space
     ("\\d\\r" . "dr")
     ;; TODO: dangerous?, will this clash with anything starting with \r?
     ("\\r" . "r")
     ("\\v" . "v")
     ("\\F" . "F"))))

(use-package f)

(use-package facet
  :load-path "~/src/facet/emacs")

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :load-path "~/src/3p/flycheck")

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package graphql-mode
  :hook (graphql-mode . (lambda ()
                          (paredit-c-mode)
                          (prettier-graphql-on-save-mode))))

(use-package haskell
  :defer t
  :bind (:map haskell-mode-map
              ("'" . self-insert-command)))

(use-package ido
  :config
  (ido-mode t)
  (ido-everywhere t)
  (setq ido-enable-flex-matching t))


(use-package ivy
  :config
  (setq ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-dynamic-exhibit-delay-ms 250)
  ;; https://oremacs.com/2018/03/05/grep-exclude/
  (setq counsel-git-cmd "rg --files"  ;;  --type py
        counsel-rg-base-command "rg -i -M 120 --no-heading --line-number --color never %s .")

  (defun -dan/swiper-around-advice (orig-fun &rest args)
    (let ((ivy-height 20))
      (apply orig-fun args)))
  (advice-add 'swiper :around '-dan/swiper-around-advice)

  (advice-add 'swiper :before (lambda (&rest args) (outline-show-all)))
  (advice-add 'swiper--ensure-visible :after 'dan/on-jump-into-buffer)
  (add-hook 'counsel-grep-post-action-hook 'dan/on-jump-into-buffer))

(use-package jedi-core
  :after (python-environment)
  :load-path "~/src/3p/emacs-jedi"
  :config
  (pyenv-mode-set "3.6.8")
  ;; TODO: set these in (use-package python-environment ...) ?
  (setq python-environment-directory (expand-file-name "~/tmp/virtualenvs")
        python-environment-virtualenv '("python" "-m" "venv"))
  (setq jedi:environment-root "emacs-jedi")
  (setq jedi:server-args '("--log" "/tmp/jediepcserver.log"
                           "--log-level" "INFO"))
  (jedi:install-server)
  (setq jedi:goto-definition-config
        '((nil definition nil)
          (t   definition nil)
          (nil nil        nil)
          (t   nil        nil)
          (nil definition t  )
          (t   definition t  )
          (nil nil        t  )
          (t   nil        t  )))
  (add-hook 'jedi:goto-definition-hook 'dan/on-jump-into-buffer))

(use-package js
  :defer t
  :bind (:map js-mode-map
              ("C-c d" . (lambda () (interactive) (insert "debugger;")))))

(use-package latex
  :defer t
  :bind (:map LaTeX-mode-map
              ("C-x n i" . dan/latex-focus-insert-comment-delimiters)
              ("C-x n r" . dan/latex-focus-remove-comment-delimiters)
              ("C-x n f" . dan/latex-focus-narrow-to-region)
              ("C-x n u" . dan/latex-unfocus)
              ("C-x n d" . dan/narrow-to-subtree)
              ("C-c |" . dan/latex-set-builder-pipe)
              ("C-c /" . dan/latex-frac-or-unfrac)
              ([(control down)] . next-line)
              ([(control up)] . previous-line)
              ([(super b)] . dan/latex-bold)
              ([(super d)] . dan/latex-definition)
              ([(super i)] . dan/latex-italic)
              ([(super t)] . dan/latex-fixed-width))
  :hook
  (LaTeX-mode . (lambda ()
                  (setq fill-column dan/fill-column
                        fci-rule-column fill-column
                        tab-width 2)
                  (setq-local indent-line-function 'dan/latex-indent-line-function)
                  (setq LaTeX-indent-environment-list (cons '("minted" . nil) LaTeX-indent-environment-list))
                  (when nil
                    (add-hook 'before-save-hook
                              (lambda () (unless (string-match ".+\\.sty" (buffer-file-name)) (dan/indent-buffer)))
                              nil 'local))))
  :config
  (setq preview-image-type 'dvipng))

(use-package lispy
  :defer t
  :bind (:map lispy-mode-map
              ("M-." . nil)
              ([(meta left)] . nil)
              ([(meta right)] . nil)))

(use-package lsp-mode
  :commands lsp
  ;; :config (require 'lsp-clients)
  )

(use-package lsp-ui
  :after lsp)

(use-package magit
  :bind (:map magit-diff-mode-map
              ([down] . nil)
              ([up] . nil)
              ([return] . magit-diff-visit-file-worktree)
              ([control return] . magit-diff-visit-file))
  :config
  (setq
   magit-completing-read-function 'ivy-completing-read
   magit-log-arguments '("--graph" "--decorate" "-n20")
   magit-push-always-verify nil
   magit-revision-insert-related-refs nil
   magit-save-repository-buffers nil
   magit-status-sections-hook '(
                                ;; magit-insert-status-headers
                                magit-insert-merge-log
                                magit-insert-rebase-sequence
                                magit-insert-am-sequence
                                magit-insert-sequencer-sequence
                                magit-insert-bisect-output
                                magit-insert-bisect-rest
                                magit-insert-bisect-log
                                ;; magit-insert-untracked-files
                                magit-insert-unstaged-changes
                                magit-insert-staged-changes
                                ;; magit-insert-stashes
                                ;; magit-insert-unpulled-commits
                                ;; magit-insert-unpushed-commits
                                ))
  (fset 'dan/magit-diff-master
        [?\C-c ?g ?d ?r ?m ?a ?s ?t ?e ?r ?. ?. ?. return]))

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("$" . dan/paired-dollar)
              ("\M-q" . fill-paragraph)
              ;; force file write to force pelican reload
              ("C-X C-s" . (lambda () (interactive) (set-buffer-modified-p t) (save-buffer)))
              ([(meta left)] . left-word)
              ([(meta right)] . right-word))
  :hook (markdown-mode . (lambda () (setq truncate-lines nil
                                     word-wrap t))))

(use-package mhtml-mode
  :defer t
  :bind (:map mhtml-mode-map
              ("C-c C-c" . emmet-expand-line))
  :hook (mhtml-mode . (lambda () (flycheck-mode -1))))

(use-package minimal
  :load-path "~/src/minimal")

(use-package modalka
  :defer t
  :bind (:map modalka-mode-map
              ((" " . modalka-mode)))
  :config
  (modalka-define-kbd "1" "C-x 1")
  (modalka-define-kbd "a" "C-a")
  (modalka-define-kbd "b" "C-x b")
  (modalka-define-kbd "d" "M-d")
  (modalka-define-kbd "^" "C-x d")
  (modalka-define-kbd "e" "C-e")
  (modalka-define-kbd "f" "C-x C-f")
  (modalka-define-kbd "g" "C-c g")
  (modalka-define-kbd "i" "M-i")
  (modalka-define-kbd "k" "C-k")
  (modalka-define-kbd "o" "M-o")
  (modalka-define-kbd "s" "C-s")
  (modalka-define-kbd "t" "C-c C-t")
  (modalka-define-kbd "u" "C-u")
  (modalka-define-kbd "x" "C-M-x")
  (modalka-define-kbd "y" "C-y")
  (modalka-define-kbd "z" "C-z")
  (modalka-define-kbd "," "s-,")
  (modalka-define-kbd "." "s-.")
  (modalka-define-kbd "[" "C-x o")
  (modalka-define-kbd "]" "C-x o")

  (modalka-global-mode -1)
  (add-to-list 'modalka-excluded-modes 'magit-status-mode)
  (add-to-list 'modalka-excluded-modes 'magit-popup-mode)
  (add-to-list 'modalka-excluded-modes 'text-mode)  ;; magit commit edit buffer
  (add-to-list 'modalka-excluded-modes 'magit-log-select-mode)
  (add-to-list 'modalka-excluded-modes 'git-rebase-mode))


(use-package ob-mathematica
  :load-path "~/src/3p/org-mode/contrib/lisp")

(use-package paredit
  :bind (:map paredit-mode-map
              ;; TODO: move into paredit-c config
              ("(" . paredit-open-round)
              ("M-[" . paredit-wrap-square)
              ("\\" . nil)
              (";" . nil)
              ;; TODO: move into paredit-c config
              ("\"" . paredit-c/doublequote)))

(use-package paredit-c
  :load-path "~/src/paredit-c")

(use-package penrose-modes
  :defer t
  :load-path "~/src/3p/penrose-modes")

(use-package projectile
  :bind-keymap
  ([(super p)] . projectile-command-map)
  :bind (:map projectile-command-map
              ("a" . dan/goto-alias-file)
              ("g" . dan/goto-gitconfig)
              ("m" . dan/goto-messages-buffer)
              ("r" . dan/goto-rustfmt-buffer))
  :config
  (projectile-mode +1)
  (setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#")
        projectile-buffers-filter-function 'projectile-buffers-with-file
        projectile-use-git-grep t
        projectile-git-command "git ls-files -zc --exclude-standard"  ;; '**/*.py' remove -o
        projectile-enable-caching t
        projectile-completion-system 'ivy
        projectile-current-project-on-switch 'keep
        projectile-switch-project-action 'counsel-git)
  (add-to-list 'projectile-globally-ignored-modes "dired-mode"))

(use-package py-isort
  :config
  (setq py-isort-options
        '("--lines=9999999"
          "--force_single_line_imports"
          "--dont-skip=__init__.py")))

(use-package pyenv-mode
  ;; :init
  ;; (add-to-list 'exec-path "~/.pyenv/shims")
  :ensure t
  :config
  (pyenv-mode))

(use-package python-environment)

(use-package reformatter
  :config
  (reformatter-define blacken
    :program "/Users/dan/bin/black"
    :args '("-l" "99" "-"))
  (reformatter-define rustfmt
    :program "/Users/dan/.cargo/bin/rustfmt")
  (reformatter-define prettier-graphql
    :program "/usr/local/bin/prettier"
    :args '("--parser" "graphql"))
  (reformatter-define prettier-html
    :program "/usr/local/bin/prettier"
    :args '("--parser" "html"))
  (reformatter-define prettier-js
    :program "/usr/local/bin/prettier"
    :args '("--parser" "babel"))
  :hook ((rust-mode . (lambda () (add-hook 'before-save-hook
                                      (lambda () (rustfmt-buffer 'display-errors)) nil t)))))

(use-package rust-mode
  :load-path "~/src/3p/rust-mode"
  :after lsp-mode
  :bind (:map rust-mode-map
              ("C-c C-c" . dan/save-even-if-not-modified)
              ("<" . dan/paired-angle-bracket)
              ("|" . dan/paired-pipe))
  :hook (
         ;; (rust-mode . lsp)
         (rust-mode . (lambda ()
                        (flycheck-mode -1)
                        (paredit-c-mode)
                        (setq fill-column dan/fill-column
                              fci-rule-column fill-column)
                        (dan/set-up-outline-minor-mode "[ \t]*\\(pub .+\\|fn .+\\|impl .+\\|struct .+\\|enum .+\\|##\\)")))
         (rust-after-save . (lambda () (flycheck-mode -1) (compile "cargo build")))))

;; (add-hook 'after-save-hook (lambda () (flycheck-mode -1) (compile "cargo build")))

(use-package tla-mode
  :defer t
  :load-path "~/src/3p/tla-mode")

(use-package toml-mode)

(use-package yasnippet
  :bind (:map yas/keymap
              ([tab] . yas/next-field))
  :config
  (yas/initialize)
  (yas/load-directory (expand-file-name "~/src/emacs-config/snippets")))


(when (file-exists-p "~/src/emacs-config/extra.el")
  (load-file "~/src/emacs-config/extra.el"))



;;; Appearance

(setq ns-use-native-fullscreen nil)
;; doesn't honor ns-use-native-fullscreen if called immediately
(run-with-timer 1 nil 'toggle-frame-fullscreen)

(setq ring-bell-function (lambda nil nil))

(setq inhibit-startup-message t)
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")

(defalias 'color-theme 'load-theme)
;; (color-theme 'railscasts-reloaded t)
;; (color-theme 'leuven t)
;; (minimal-mode)
(tool-bar-mode -1)

(defun dan/set-appearance ()
  (interactive)
  (scroll-bar-mode -1)
  (set-cursor-color "red")
  (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
  (blink-cursor-mode -1)

  (set-face-background 'fringe (face-background 'default))
  (dan/set-show-paren-style))

(defun dan/set-show-paren-style ()
  (show-paren-mode t)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil :foreground "red"))

(dan/set-appearance)

(advice-add 'load-theme :after (lambda (&rest args) (dan/set-appearance)))

;;; Modes
(add-to-list 'auto-mode-alist '("\\.applescript\\'" . applescript-mode))
(add-to-list 'auto-mode-alist '("\\.compilation\\'" . compilation-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jira\\'" . jira-markup-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.sublime-syntax\\'" . yaml-mode))

;;; Server

(require 'server)
(unless (server-running-p) (server-start))


;;; Etc
(setq
 async-shell-command-buffer 'rename-buffer
 auto-save-default nil
 create-lockfiles nil
 electric-indent-mode nil
 enable-recursive-minibuffers nil
 initial-scratch-message nil
 kill-read-only-ok t
 make-backup-files nil
 save-silently t
 shell-command-default-error-buffer "*Shell Command Error*"
 vc-follow-symlinks t)

(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)


(defvar dan/before-revert-data)
(defun dan/before-revert-hook-fn ()
  (setq dan/before-revert-data `(:point-min ,(point-min) :point-max ,(point-max))))
(add-hook 'before-revert-hook 'dan/before-revert-hook-fn)

(defun dan/after-revert-hook-fn ()
  (when dan/before-revert-data
    (let ((point-min-before (plist-get dan/before-revert-data :point-min))
          (point-max-before (plist-get dan/before-revert-data :point-max)))
      (unless (and (eq (point-min) point-min-before)
                   (eq (point-max) point-max-before))
        (narrow-to-region point-min-before point-max-before)))))
(add-hook 'after-revert-hook 'dan/after-revert-hook-fn)

(use-package autorevert
  :config
  (global-auto-revert-mode t)
  (setq auto-revert-interval 0.1
        global-auto-revert-non-file-buffers t))

(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(set-default 'tab-width 4)

(dan/set-exec-path-from-shell)
(advice-add 'message :after 'dan/message-buffer-goto-end-of-buffer)


(setq dan/fill-column 99)
(use-package fill-column-indicator
  :config
  (setq fci-rule-column dan/fill-column
        fci-rule-color "#A5BAF1"))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(winner-mode t)
(require 'ido)
(let ((is-dired-buffer? (lambda (buff) (eq (with-current-buffer buff major-mode) 'dired-mode))))
  (add-to-list 'ido-ignore-buffers is-dired-buffer?)
  (add-to-list 'winner-boring-buffers is-dired-buffer?))

(windmove-default-keybindings)

(setq tramp-verbose 2)
(setq tramp-default-method "ssh")

(recentf-mode t)
(setq recentf-max-saved-items nil)
(add-to-list 'recentf-exclude ".*\\.\\(png\\|pdf\\)")

(advice-add 'goto-line :before (lambda (&rest args) (outline-show-all)))

(delete-selection-mode t)

(defun pop-to-mark-command ()
  (dan/pop-to-mark-command))

(setq css-indent-offset 2)

;;; Bookmarks
(setq bookmark-bmenu-file-column 80)
(setq bookmark-sort-flag nil)
(setq bookmark-save-flag 20)

;; message overwrites useful messages
(defun bookmark-maybe-message (fmt &rest args))

;; (advice-add 'revert-buffer :around (symbol-function 'save-excursion))


;;; Search
(setq ag-arguments
      '("--line-number" "--smart-case" "--nogroup" "--column" "--"))
(setq dan/extra-ag-arguments
      '("--follow"
        "--ignore" "*/migrations/*"
        "--ignore" "*/logs/*"
        "--ignore" "*/thirdparty/*"))


;;; Scratch buffers
(setq dan/scratch-buffer-dir "/tmp")


;;; scrolling
(setq scroll-conservatively 101)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq linum-delay t)

;;; Comint

(advice-add 'comint-previous-matching-input-from-input :after (lambda (&rest args) (goto-char (point-at-eol))))


;;; Multiple Cursors
(advice-add 'mc/edit-lines :before (lambda (&rest args) (previous-logical-line 1 nil)))


;; Haskell
(setq hindent-extra-args '("--line-length" "100"))

;; Agda
(when nil
  (load-file (let ((coding-system-for-read 'utf-8))
               (shell-command-to-string "agda-mode locate")))
  ((setq agda2-highlight-level 'non-interactive)))


;;; Org
(setq org-src-fontify-natively t)
(setq org-edit-src-persistent-message nil)
(setq org-src-window-setup 'current-window)
(setq org-todo-keywords '((sequence "TODO" "DEFER" "WONTDO" "DONE")))

(setq org-confirm-babel-evaluate nil)
(require 'ob-haskell)
(set-face-attribute 'org-block-begin-line nil :foreground "lightgrey")
(set-face-attribute 'org-block-end-line nil :foreground "lightgrey")


;;; Erc

;;; Plantuml
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.11/libexec/plantuml.jar")


;;; Python
(defvar-local dan/python-project-name nil
  "The name of the current python project.

Suppose the name is $name. The following statements are true:
1. The project is held in a git repository at a path like .../$name/.git.
2. The full path is known to projectile.
3. The project virtualenv is a directory also named $name. Its
   parent directory is python-environment-directory.")

(defvar-local dan/python-virtualenv nil
  "Absolute path to python virtualenv for current buffer.")


(defvar-local dan/python-project-root nil
  "Absolute path to project root.
The project root is the place where you might find tox.ini, setup.py, Makefile, etc.")


(defun dan/python-mode-hook-fn ()
  (interactive)

  (setq dan/python-project-name (dan/python-infer-project-name)
        dan/python-virtualenv (dan/python-infer-virtualenv dan/python-project-name)
        dan/python-project-root (dan/python-infer-project-root dan/python-project-name))

  (if (and dan/python-virtualenv
           dan/python-project-root)
      (progn
        (let* ((config
                '(;; Project
                  (dan/python-project-name . dan/python-project-name)
                  (dan/python-virtualenv . dan/python-virtualenv)
                  (dan/python-project-root . dan/python-project-root)

                  ;; Flycheck
                  (flycheck-flake8-maximum-line-length . 99)
                  (flycheck-highlighting-mode . 'lines)
                  (flycheck-python-flake8-executable . (f-join dan/python-virtualenv "bin/flake8"))
                  (flycheck-python-mypy-executable . (f-join dan/python-virtualenv "bin/mypy"))
                  (flycheck-flake8rc . (f-join dan/python-project-root "tox.ini"))
                  (flycheck-python-mypy-ini . (f-join dan/python-project-root "tox.ini"))

                  ;; Shell
                  (python-shell-virtualenv-root . dan/python-virtualenv)
                  (python-shell-interpreter . (f-join dan/python-virtualenv "bin/ipython"))
                  (python-shell-interpreter-args . "-i"))))

          (mapcar (lambda (pair) (set (make-variable-buffer-local (car pair)) (eval (cdr pair))))
                  config)
          (set (make-variable-buffer-local 'dan/python-buffer-config-keys)
               (mapcar 'car config)))

        ;; (assert (f-directory? dan/python-virtualenv) t)
        ;; (assert (f-directory? dan/python-project-root) t)
        ;; (assert (f-executable? flycheck-python-flake8-executable) t)
        ;; (assert (f-executable? flycheck-python-mypy-executable) t)
        ;; (assert (f-file? flycheck-flake8rc) t)
        ;; (assert (f-file? flycheck-python-mypy-ini) t)
        ;; (assert (f-directory? python-shell-virtualenv-root) t)
        ;; (assert (f-executable? python-shell-interpreter) t)

        (setf (flycheck-checker-get 'python-flake8 'next-checkers) '((t . python-mypy)))
        (setf (flycheck-checker-get 'python-mypy 'next-checkers) nil)

        (flycheck-select-checker 'python-flake8))
    (message "Python virtualenv / project root are unknown"))

  (company-mode)
  (jedi:setup)

  (setq fill-column 99)
  (set (make-variable-buffer-local 'fci-rule-column) fill-column)

  (setq python-fill-docstring-style 'django)
  (paredit-c-mode)
  (set (make-variable-buffer-local 'prettify-symbols-alist)
       '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))


(add-hook 'python-mode-hook 'dan/python-mode-hook-fn 'append)







(defun dan/get-default-directory-from-python-shell-maybe ()
  (let ((process (python-shell-get-process)))
    (if process
        (with-current-buffer (process-buffer process)
          default-directory)
      default-directory)))

(advice-add 'python-eldoc-function
            :around
            (lambda (orig-func &rest args)
              (let ((default-directory (dan/get-default-directory-from-python-shell-maybe)))
                (apply orig-func args))))

(advice-add 'python-shell-send-string
            :around
            (lambda (orig-func &rest args)
              (let ((default-directory (dan/get-default-directory-from-python-shell-maybe)))
                (apply orig-func args)

                ;; This could be nice to get rid of the active region after
                ;; evaluating code, but I think python-shell-send-string is
                ;; called in the background by a timer, so need to not do it then.
                ;; (deactivate-mark)
                )))

(when nil
  (advice-add 'python-shell-completion-native-get-completions
              :around
              (lambda (orig-func process import input)
                (when (or import input)
                  (apply orig-func (list process import input))))))


(defvar dan/python-misc-dir "~/src/misc")

;; (add-function :before (symbol-function 'run-python) 'dan/python-set-virtualenv)
;; (remove-function (symbol-function 'run-python) 'dan/python-set-virtualenv)

;; Python comint history
(defvar dan/python-comint-history-file "~/.ipython/history")
(defvar dan/python-comint-history-max 1000)

(defun dan/load-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  (message "Loading python comint history...")
  (mapc (lambda (item) (ring-insert+extend comint-input-ring item 'grow))
        (dan/read-comint-history file))
  (message "done"))

(defun dan/read-comint-history (file)
  (split-string (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)) "\n" t))

(defun dan/dump-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  ;; Most recent is first in comint-input-ring. Write file in
  ;; same order seeing as we are overwriting, not appending.
  (let ((history (org-uniquify (ring-elements comint-input-ring))))
    (setq history (subseq history 0 (min (length history)
                                         dan/python-comint-history-max)))
    (with-temp-buffer
      (insert (mapconcat #'identity history "\n") "\n")
      (write-file file))))


(add-hook 'kill-buffer-hook
          (lambda () (when (eq major-mode 'inferior-python-mode)
                  (dan/dump-comint-history dan/python-comint-history-file))))

(add-hook 'inferior-python-mode-hook
          (lambda () (dan/load-comint-history dan/python-comint-history-file)))

;;; Mode hooks
(setq pulse-iterations 40)

(defun dan/on-jump-into-buffer ()
  (outline-show-all) ;; with swiper--ensure-visible this leaves point in the wrong place
  (dan/pulse-momentary-highlight-current-line)
  (when (eq major-mode 'python-mode)
    (dan/python-current-defun-name)))

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)

(defun dan/before-save-hook-fn ()
  (dan/query-delete-trailing-whitespace)
  (when (or (eq major-mode 'org-mode) (eq major-mode 'markdown-mode))
    (dan/org-table-to-markdown)))
(add-hook 'before-save-hook 'dan/before-save-hook-fn)

(defun dan/awk-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'awk-mode-hook 'dan/awk-mode-hook-fn)


(defun dan/c-mode-hook-fn ()
  (setq c-basic-offset 4)
  (paredit-c-mode))
(add-hook 'c-mode-hook 'dan/c-mode-hook-fn)

(defun dan/clojure-mode-hook-fn ()
  (paredit-c-mode)
  (inf-clojure-minor-mode))
(add-hook 'clojure-mode-hook 'dan/clojure-mode-hook-fn)
(add-hook 'clojurescript-mode-hook 'dan/clojure-mode-hook-fn)


(defun dan/dired-mode-hook-fn ()
  (dired-omit-mode))
(add-hook 'dired-mode-hook 'dan/dired-mode-hook-fn)


(defun dan/inf-clojure-mode-hook-fn ()
  (paredit-mode))
(add-hook 'inf-clojure-mode-hook 'dan/inf-clojure-mode-hook-fn)

(defun dan/coffee-mode-hook-fn ()
  (paredit-c-mode)
  (setq coffee-tab-width 2))
(add-hook 'coffee-mode-hook 'dan/coffee-mode-hook-fn)

(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (company-mode)
  (setq fill-column dan/fill-column)
  (set (make-variable-buffer-local 'fci-rule-column) fill-column)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)")
  (add-hook 'before-save-hook (lambda () (indent-region (point-min) (point-max))) nil t))
(add-hook 'emacs-lisp-mode-hook 'dan/emacs-lisp-mode-hook-fn)

(defun dan/eshell-mode-hook-fn ()
  (paredit-mode t)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (prettify-symbols-mode))
(add-hook 'eshell-mode-hook 'dan/eshell-mode-hook-fn)

(defun dan/find-function-after-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'find-function-after-hook 'dan/find-function-after-hook-fn)

(defun dan/haskell-mode-fn ()
  (hindent-mode)
  ;; (add-hook 'after-save-hook 'hindent-reformat-buffer nil t)
  (paredit-c-mode)
  (local-set-key "'" 'self-insert-command))
(add-hook 'haskell-mode-hook 'dan/haskell-mode-fn)


(defun dan/html-mode-hook-fn ())
(add-hook 'html-mode-hook 'dan/html-mode-hook-fn)

(defun dan/inferior-python-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'inferior-python-mode-hook 'dan/inferior-python-mode-hook-fn)

(defun dan/js-mode-hook-fn ()
  (paredit-c-mode)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'dan/js-mode-hook-fn)





(defun dan/latex-mode-hook-fn ()
  (interactive)
  (dan/latex-paired-characters)

  (dan/set-up-outline-minor-mode "\\(\\\\sub\\|\\\\section\\|\\\\begin\\|\\\\item\\)")

  ;; (dan/watch-mathematics)
  (add-to-list 'LaTeX-item-list
               '("align" . dan/latex-insert-item-in-align-environment))
  (add-to-list 'LaTeX-item-list
               '("align*" . dan/latex-insert-item-in-align-environment))

  (setq fill-column dan/fill-column)
  (setq fci-rule-column fill-column)

  (setq org-latex-packages-alist '(("" "mathematics" t))
        ;; let mathematics.sty specify all packages
        org-latex-default-packages-alist nil)
  ;; (xenops-mode)
  (setq xenops-image-latex-template
        "\\begin{mdframed}\n\\includegraphics[width=400pt]{%s}\n\\end{mdframed}"))

(add-hook 'LaTeX-mode-hook 'dan/latex-mode-hook-fn)

(defun dan/magit-diff-mode-hook-fn ()
  (dan/magit-hide-all-sections))
(add-hook 'magit-diff-mode-hook 'dan/magit-diff-mode-hook-fn)

(add-hook 'magit-diff-visit-file-hook 'dan/on-jump-into-buffer)

(defun dan/makefile-mode-hook-fn ()
  (paredit-c-mode)
  (local-set-key "\C-j" #'electric-newline-and-maybe-indent))
(add-hook 'makefile-mode-hook 'dan/makefile-mode-hook-fn)

(defun dan/minibuffer-setup-hook-fn ()
  (when (eq this-command 'eval-expression)
    (setq completion-at-point-functions '(lisp-completion-at-point t))
    (local-set-key [tab] 'complete-symbol)
    (local-set-key "/" 'self-insert-command)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'dan/minibuffer-setup-hook-fn)

(defun dan/next-error-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'next-error-hook 'dan/next-error-hook-fn)

(defun dan/occur-mode-find-occurrence-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'occur-mode-find-occurrence-hook 'dan/occur-mode-find-occurrence-hook-fn)

(defun dan/org-mode-hook-fn ()
  (local-set-key [(meta left)] 'backward-word)
  (local-set-key [(meta right)] 'forward-word)
  (local-set-key [(shift left)] 'windmove-left)
  (local-set-key [(shift right)] 'windmove-right))
(add-hook 'org-mode-hook 'dan/org-mode-hook-fn)

(defun dan/paredit-c-mode-hook-fn ()
  (local-set-key [(meta up)] 'dan/transpose-line-up)
  (local-set-key [(meta down)] 'dan/transpose-line-down)
  (local-set-key ";" 'self-insert-command))
(add-hook 'paredit-c-mode-hook 'dan/paredit-c-mode-hook-fn)


(when nil
  (add-to-list 'load-path "~/src/3p/penrose-modes") (require 'penrose-modes)
  (defun dan/penrose-hook-fn ()
    (paredit-c-mode))

  (add-hook 'penrose-substance-mode-hook 'dan/penrose-hook-fn)
  (add-hook 'penrose-style-mode-hook 'dan/penrose-hook-fn)
  (add-hook 'penrose-dsl-mode-hook 'dan/penrose-hook-fn))

(defun dan/r-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'ess-mode-hook 'dan/r-mode-hook-fn)
(add-hook 'inferior-ess-mode-hook 'dan/r-mode-hook-fn)

(defun dan/scheme-mode-hook-fn ()
  (paredit-mode))
(add-hook 'scheme-mode-hook 'dan/scheme-mode-hook-fn)

(defun dan/sh-mode-hook-fn ()
  (setq sh-indentation 4)
  (setq sh-basic-offset nil)
  (dan/set-up-outline-minor-mode "[a-zA-Z_-]+[ \t]*(")
  (paredit-c-mode))
(add-hook 'sh-mode-hook 'dan/sh-mode-hook-fn)


(require 'sql-indent)
;; See https://github.com/alex-hhh/emacs-sql-indent/blob/master/sql-indent.org
;; and `sqlind-indentation-offsets-alist'
(defvar dan/sqlind-offsets-alist
  `((select-clause 0)
    (select-table-continuation sqlind-indent-select-table sqlind-lineup-joins-to-anchor 0)
    ,@sqlind-default-indentation-offsets-alist))

(defun dan/sql-mode-hook-fn ()
  (paredit-c-mode)
  (sqlind-minor-mode)
  (setq sqlind-indentation-offsets-alist dan/sqlind-offsets-alist)
  (add-hook 'before-save-hook 'dan/indent-region nil t))


(add-hook 'sql-mode-hook 'dan/sql-mode-hook-fn)

(defun dan/yaml-mode-hook-fn ()
  (dan/set-up-outline-minor-mode "[^ \t]+"))
(add-hook 'yaml-mode-hook 'dan/yaml-mode-hook-fn)


;;; Spam

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "4e5e58e42f6f37920b95a8502f488928b3dab9b6cc03d864e38101ce36ecb968" "72759f4e42617df7a07d0a4f4b08982314aa97fbd495a5405c9b11f48bd6b839" "9e6ac467fa1e5eb09e2ac477f61c56b2e172815b4a6a43cf48def62f9d3e5bf9" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3b4800ea72984641068f45e8d1911405b910f1406b83650cbd747a831295c911" default)))
 '(default-input-method "latin-1-prefix")
 '(magit-diff-arguments (quote ("--ignore-all-space" "--no-ext-diff")))
 '(package-selected-packages
   (quote
    (elisp-format company-lean lean-mode sql-indent material-theme graphql-mode typescript-mode reformatter lsp-rust cargo flycheck-rust toml-mode lsp-ui wgrep ace-jump-mode ace-window forge applescript-mode auctex auctex-latexmk aumix-mode auto-overlays avy buffer-move coffee-mode color-theme-modern color-theme-railscasts company company-jedi confluence counsel debbugs dired-details+ dockerfile-mode dot-mode emmet-mode ess eyuml f fill-column-indicator fzf graphviz-dot-mode haskell-mode hindent htmlize ivy jira-markup-mode latex-pretty-symbols magit markdown-mode minimal-theme modalka multiple-cursors paredit paredit-everywhere plantuml-mode pony-mode projectile pyenv-mode py-isort railscasts-reloaded-theme railscasts-theme ripgrep smartparens smooth-scroll soothe-theme sqlite sublimity transpose-frame use-package visual-fill-column yaml-mode yasnippet yasnippet-bundle zencoding-mode zones)))
 '(safe-local-variable-values (quote ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:weight bold))))
 '(font-latex-math-face ((t (:foreground "red"))))
 '(font-latex-string-face ((t (:foreground "black"))))
 '(font-latex-verbatim-face ((t (:inherit nil))))
 '(help-argument-name ((t (:inherit nil))))
 '(highlight ((t (:background "yellow" :foreground "black"))))
 '(minibuffer-prompt ((t (:background "gray99" :foreground "gray35" :weight bold))))
 '(org-block ((t (:foreground "#000088"))))
 '(org-block-begin-line ((t (:foreground "lightgrey" :underline nil))))
 '(org-block-end-line ((t (:foreground "lightgrey" :overline nil))))
 '(org-done ((t (:background "palegreen" :foreground "darkgrey" :box (:line-width 1 :color "grey") :weight normal))))
 '(org-level-1 ((t (:background nil :foreground "#CC7733" :overline nil :weight bold :height 120))))
 '(org-level-2 ((t (:background nil :foreground "dark red" :overline nil :weight bold :height 120))))
 '(org-todo ((t (:foreground "darkgrey" :box (:line-width 1 :color "grey") :weight normal)))))
(put 'upcase-region 'disabled nil)

(put 'LaTeX-narrow-to-environment 'disabled nil)
(message "⚡")
