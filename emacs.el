;; -*- lexical-binding: t -*-

;;; Init
(setq debug-on-error nil)

;;; Package manager

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(setq use-package-always-demand t)

;;; Native packages

(use-package emacs
  :bind (("C-M-\\" . dan/indent-region)
         ("C-b" . backward-sexp)
         ("C-l" . dan/message-buffer-clear)
         ("C-c 1" . flycheck-mode)
         ("C-c C-c" . dan/save-even-if-not-modified)
         ("C-c C-l" . (lambda () (interactive) (eval-buffer) (message "eval-buffer: %s" (buffer-file-name))))
         ("C-c C-r" . magit-file-rename)
         ("C-c C-z" . python-shell-switch-to-shell)
         ("C-c M-d" . dan/magit-diff)
         ("C-c M-f" . search-files-thing-at-point)
         ("C-c b" . magit-blame)
         ("C-c c" . (lambda () (interactive) (magit-show-commit "HEAD")))
         ("C-c d" . dan/debug-on-error)
         ("C-c e" . outline-show-all)
         ("C-c f" . dan/describe-face-at-point)
         ("C-c g" . magit-status)
         ("C-c l" . linum-mode)
         ("C-c m" . dan/display-messages-buffer)
         ("C-c o" . dan/scratch-buffer)
         ("C-c r" . (lambda (&optional arg) (interactive "P") (call-interactively (if arg #'dan/projectile-replace #'replace-regexp))))
         ("C-c s" . (lambda () (interactive) (shell-command-on-region (region-beginning) (region-end) "sort -V" nil 'replace)))
         ("C-c w" . dan/list-window-configurations)
         ("C-c x" . (lambda () (interactive) (find-library "xenops")))
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
         ("C-x C-s" . dan/save-buffer)
         ("M-i" . dan/highlight)
         ("M-o" . (lambda () (interactive) (swiper (thing-at-point 'symbol))))
         ("M-q" . fill-paragraph)
         ([f1] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?1 arg)))
         ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
         ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
         ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
         ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
         ([f6] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?6 arg)))
         ([f7] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?7 arg)))
         ([f8] . magit-log)
         ([f9] . dan/magit-dev-mode)
         ([f10] . (lambda () (interactive) (find-file "~/dandavison7@gmail.com/Projects/xenops.org")))
         ([f11] . dan/goto-emacs-config)
         ([f12] . (lambda () (interactive) (switch-to-buffer "*eshell*")))
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
         ([(super c)] . kill-ring-save)
         ([(super d)] . dan/bookmark-set)
         ([(super f)] . dan/find-file)
         ([(super g)] . magit-status)
         ([(super k)] . dan/bookmark-dwim)
         ([(super left)] . winner-undo)
         ([(super l)] . find-library)
         ([(super m)] . dan/goto-definition)
         ([(super n)] . (lambda () (interactive) (dan/switch-to-buffer '(4))))
         ([(super o)] . dan/find-file)
         ([(super return)] . dan/maximize)
         ([(super right)] . winner-redo)
         ([(super r)] . projectile-replace)
         ([(super s)] . dan/save-buffer) ;; not bound in MacOS port
         ([(super u)] . revert-buffer) ;; not bound in MacOS port
         ([(super v)] . yank) ;; not bound in MacOS port
         ([(super w)] . widen)
         ([(super x)] . kill-region)
         ([(super ?&)] . (lambda () (interactive) (let ((kill-buffer-query-functions nil)) (kill-buffer))))
         ([(super ?,)] . dan/counsel-rg)
         ([(super ?.)] . dan/counsel-rg-thing-at-point)
         ([(super ?')] . magit-dispatch)
         ([(super ?\;)] . dan/show-buffer-file-name)
         ([(super ?\])] . fci-mode)
         ([(super \\)] . dan/indent-region)
         ([(super |)] . dan/shell-command-on-region-and-replace)))

;;; MacOS
;; https://github.com/railwaycat/homebrew-emacsmacport/
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(use-package bookmark
  :defer t
  :bind (:map bookmark-bmenu-mode-map
         ("C-x C-s" . bookmark-save)))

(use-package cl)


(use-package comint
  :defer t
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
  :defer t
  :bind (:map dired-mode-map
         ("A" . (lambda () (interactive)
                  (shell-command (format "git add %s" (dired-get-filename)))
                  (magit-status)))
         ([(super r)] . magit-file-rename)
         ;; ("R" . (lambda () (interactive) (dan/magit-file-rename (dired-filename-at-point))))
         ([left] . dired-up-directory)
         ([right] . dired-find-file))
  :config
  (setq dired-auto-revert-buffer t
        dired-omit-size-limit nil
        dired-omit-files "^__pycache__$\\|\\.egg-info$\\|^.mypy_cache$\\|^\\.\\.?\\|\\.pyc\\|^junk\\."
        dired-omit-verbose nil)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq-default dired-omit-files-p t)
  :hook ((dired-mode . (lambda ()
                         (dired-omit-mode)
                         (dired-hide-details-mode 1)
                         (push (buffer-name) winner-boring-buffers)))))

(use-package dired-x)

(use-package erc
  :defer t
  :config
  (setq erc-nick "dd7"
        erc-hide-list '("JOIN" "PART" "QUIT")))

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
         ("C-c d" . edebug-defun)
         ("C-c ," . find-function)
         ("C-c C-r" . (lambda () (interactive) (call-interactively 'eval-region) (deactivate-mark)))
         ([(super x)] . eval-defun)
         ([(meta left)] . nil)
         ([(meta right)] . nil))
  :config
  (defun dan/emacs-lisp-mode-hook-fn ()
    (interactive)
    (paredit-mode t)
    (company-mode)
    (setq fill-column dan/fill-column)
    (set (make-variable-buffer-local 'fci-rule-column) fill-column)
    (setq prettify-symbols-alist '(("lambda" . 955)))
    (prettify-symbols-mode)
    (dan/set-up-outline-minor-mode "\\((\\|;;;\\)")
    ;;(add-hook 'before-save-hook (lambda () (indent-region (point-min) (point-max))) nil t)
    )

  :hook (emacs-lisp-mode . dan/emacs-lisp-mode-hook-fn))

(use-package org
  :defer t
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
  :defer t
  :bind (:map outline-minor-mode-map
         ([(control tab)] . org-cycle)
         ([(backtab)] . org-global-cycle)))

(use-package python
  :defer t
  :bind (:map python-mode-map
         ("C-c d" . dan/python-insert-ipdb-set-trace)
         ("C-c C-c" . dan/save-even-if-not-modified)
         ("?" . dan/python-question-mark)
         ([(super ?b)] . dan/python-black)
         ([(super i)] . dan/python-where-am-i)
         ([(meta shift right)] . python-indent-shift-right)
         ([(meta shift left)] . python-indent-shift-left)
         ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (xref-find-definitions))))
  :config
  (load-file "~/src/emacs-config/python.el")
  :hook (python-mode . dan/python-mode-hook-fn))

(load-file "~/src/emacs-config/lib.el")

(use-package term
  :defer t
  :hook (term-mode . eterm-256color-mode))

;;; External Packages

(use-package ace-window
  :defer t
  :bind (("C-x o" . ace-window)))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(defun dan/java-mode-hook-function ()
  (paredit-c-mode)
  (meghanada-mode t)
  (meghanada-telemetry-enable t)
  (flycheck-mode +1)
  (setq c-basic-offset 2)
  (add-hook 'before-save-hook (lambda () (indent-region (point-min) (point-max))) nil t)
  (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))

(use-package cc-mode
  :hook (java-mode . dan/java-mode-hook-function))

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

(use-package dash)

(use-package eglot
  :load-path "~/src/3p/eglot"
  :config
  (setq eglot-transform-path-function #'dan/eglot-pyls-transform-path-to-docker-tramp))

(use-package eglot-x
  :load-path "~/src/3p/eglot-x"
  :after eglot)

(use-package f)

(use-package facet
  :load-path "~/src/facet/emacs")

(use-package flycheck)

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
  (require 'counsel)
  (require 'swiper)
  (require 'hydra)
  (setq ivy-use-virtual-buffers t
        ivy-fixed-height-minibuffer t
        ivy-height 20
        ivy-dynamic-exhibit-delay-ms 250)
  ;; https://oremacs.com/2018/03/05/grep-exclude/
  (setq counsel-git-cmd "rg --files"  ;;  --type py
        counsel-rg-base-command "rg -i -M 512 --no-heading --line-number --color never %s .")

  ;; Doesn't seem to work well unfortunately
  ;; (advice-add 'ivy-previous-line :after #'ivy-call)
  ;; (advice-add 'ivy-next-line :after #'ivy-call)

  (defun -dan/swiper-around-advice (orig-fun &rest args)
    (let ((ivy-height 20))
      (apply orig-fun args)))
  (advice-add 'swiper :around '-dan/swiper-around-advice)
  (advice-add 'swiper :before (lambda (&rest args) (outline-show-all)))
  (advice-add 'swiper--ensure-visible :after 'dan/on-jump-into-buffer)
  (add-hook 'counsel-grep-post-action-hook 'dan/on-jump-into-buffer))


(use-package js
  :defer t
  :bind (:map js-mode-map
         ("C-c d" . (lambda () (interactive) (insert "debugger;")))))

(use-package latex
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
  :config
  (load-file "~/src/emacs-config/latex.el")

  :hook
  (latex-mode-hook . dan/latex-mode-hook-fn)
  (LaTeX-mode-hook . dan/latex-mode-hook-fn)
  (plain-tex-mode-hook . dan/latex-mode-hook-fn))

(dolist (hook '(plain-TeX-mode-hook latex-mode-hook LaTeX-mode-hook))
  (add-hook hook #'dan/latex-mode-hook-fn))

(use-package override-lisp-indent
  :load-path "~/src/3p/override-lisp-indent")

(use-package xenops
  :load-path "~/src/xenops/lisp"
  ;; :bind (:map xenops-mode-map
  ;;        ("C-p" . xenops-xen-mode)
  ;;        ("C-x c" . xenops-avy-copy-math-and-paste))
  ;; :config
  ;; (load-file "~/src/emacs-config/xenops.el")

  ;; :hook
  ;; (xenops-mode-hook
  ;;  .
  ;;  (lambda ()
  ;;    (dan/set-up-outline-minor-mode "\\(\\\\sub\\|\\\\section\\|\\\\begin\\|\\\\item\\)")))
  )

(use-package lispy
  :defer t
  :bind (:map lispy-mode-map
         ("M-." . nil)
         ([(meta left)] . nil)
         ([(meta right)] . nil)))

(when nil
  (use-package lsp-mode
    :hook ((python-mode . lsp))
    :commands lsp
    :config
    ;; https://github.com/emacs-lsp/lsp-mode#performance
    (setq gc-cons-threshold 100000000)
    (setq read-process-output-max (* 1024 1024))
    (setq lsp-prefer-capf t)
    (setq lsp-idle-delay 0.500))

  (use-package lsp-ui
    :after lsp))

(use-package magit
  :load-path "~/src/3p/magit/lisp"
  :bind (:map magit-diff-mode-map
         ([down] . nil)
         ([up] . nil)
         ([return] . magit-diff-visit-worktree-file)
         ([control return] . magit-diff-visit-file)
         :map with-editor-mode-map
         ("C-x C-$" . dan/magit-commit))

  :config

  ;; (transient-append-suffix 'magit-diff "d"
  ;; '("m" "Master" (lambda () (magit-diff-range "master..."))))

  ;; (transient-remove-suffix 'magit-diff "d")

  (when t
    (setq
     magit-diff-refine-hunk 'all
     magit-completing-read-function 'ivy-completing-read
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
                                  )))
  (fset 'dan/magit-diff-master
        [?\C-c ?g ?d ?r ?m ?a ?s ?t ?e ?r ?. ?. ?. return]))


(if nil
    (use-package magit-delta
      :after magit xterm-color
      :load-path "~/src/magit-delta"
      :config
      (magit-delta-mode +1))
  (package-install-file "~/src/3p/melpa/packages/magit-delta-20200423.327.el")
  (magit-delta-mode +1))


(use-package markdown-mode
  :bind (:map markdown-mode-map
         ("$" . dan/paired-dollar)
         ("\M-q" . fill-paragraph)
         ;; force file write to force pelican reload
         ("C-X C-s" . (lambda () (interactive) (set-buffer-modified-p t) (save-buffer)))
         ([(meta left)] . left-word)
         ([(meta right)] . right-word))
  :config
  (advice-add 'yank :after (lambda (&rest args)
                             (if (eq major-mode 'markdown-mode)
                                 (save-excursion (skip-chars-backward " \n")
                                                 (dan/markdown-image-to-html (point-at-bol) (point-at-eol))))))
  (setq markdown-fontify-code-blocks-natively t)
  :hook (markdown-mode . (lambda () (setq truncate-lines nil
                                     word-wrap t))))


(use-package meghanada
  :defer t
  :config
  (setq meghanada-java-path "java")
  (setq meghanada-maven-path "mvn"))

(use-package mhtml-mode
  :defer t
  :bind (:map mhtml-mode-map
         ("C-c C-c" . emmet-expand-line)))

(use-package minimal
  :load-path "~/src/minimal")

(when nil
  (use-package mma
    :defer t
    :load-path "~/src/3p/mma-mode"
    :config
    (add-to-list 'auto-mode-alist '("\\.m\\'" . mma-mode))
    ;; (font-lock-add-keywords
    ;;  'mma-mode
    ;;  '(("\\<\\([^ [\n]+\\)\\[" 1 'font-lock-function-name-face)))
    (defun dan/mma-mode-hook-fn ()
      (paredit-c-mode))
    :hook
    (mma-mode-hook . #'dan/mma-mode-hook-fn)))

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

(use-package neotree
  :bind (:map neotree-mode-map
         ([(left)] . neotree-select-up-node)
         ([(right)] . (lambda () (interactive) (execute-kbd-macro (kbd "<tab>"))))))

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
         ("m" . dan/display-messages-buffer)
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

(use-package pyenv-mode :defer t)

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
  :after lsp-mode
  :bind (:map rust-mode-map
         ("C-c C-c" . dan/save-even-if-not-modified)
         ("<" . dan/paired-angle-bracket)
         ("|" . dan/paired-pipe))
  :hook (
         ;; (rust-mode . lsp)
         (rust-mode . (lambda ()
                        (paredit-c-mode)
                        (setq fill-column dan/fill-column
                              fci-rule-column fill-column)
                        (dan/set-up-outline-minor-mode "[ \t]*\\(pub .+\\|fn .+\\|impl .+\\|struct .+\\|enum .+\\|##\\)")))
         (rust-after-save . (lambda () (compile "cargo build")))))

(use-package swift-mode
  :defer t
  :hook (
         (swift-mode . (lambda ()
                         (paredit-c-mode)
                         (setq fill-column dan/fill-column
                               fci-rule-column fill-column)
                         (dan/set-up-outline-minor-mode "[ \t]*\\(public .+\\|func .+\\|enum .+\\)")))))

(use-package tla-mode
  :defer t
  :load-path "~/src/3p/tla-mode")

(use-package toml-mode)

(use-package xterm-color
  :config
  ;; https://github.com/atomontage/xterm-color#usage
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun dan/compilation-filter-advice (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'dan/compilation-filter-advice))

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

;; (dan/theme-load 'railscasts-reloaded)
;; (dan/theme-load 'minimal-black)
;; (dan/theme-load 'leuven)
;; (minimal-mode)
(tool-bar-mode -1)
(dan/set-appearance)

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
(setq-default lexical-binding t)
(setq
 uniquify-min-dir-content 1
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
 vc-follow-symlinks t
 find-function-C-source-directory nil ;;(f-expand "~/src/3p/emacs/src")
 sentence-end-double-space nil)

(setq save-some-buffers-default-predicate (lambda ())) ;; this might cause problems? maybe in magit?

;; (desktop-save-mode 1)
(add-to-list 'desktop-globals-to-save 'command-history)

(setenv "INFOPATH" "/Users/dan/src/3p/emacs-mac/share/info")
(add-to-list 'Info-additional-directory-list "/Users/dan/src/3p/emacs-mac/share/info")

(fset 'yes-or-no-p 'y-or-n-p)

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



(use-package fill-column-indicator
  :config
  (dan/set-fill-column 80))

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(require 'ido)
(let ((is-dired-buffer? (lambda (buff) (eq (with-current-buffer buff major-mode) 'dired-mode))))
  (add-to-list 'ido-ignore-buffers is-dired-buffer?))

(winner-mode t)
(windmove-default-keybindings)

(setq tramp-verbose 2)
(setq tramp-default-method "ssh")

(recentf-mode t)
(setq recentf-max-saved-items nil)
(add-to-list 'recentf-exclude ".*\\.\\(png\\|pdf\\)")
(add-to-list 'recentf-exclude "/var/.*")

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
  (recenter)
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


(defun dan/inf-clojure-mode-hook-fn ()
  (paredit-mode))
(add-hook 'inf-clojure-mode-hook 'dan/inf-clojure-mode-hook-fn)

(defun dan/coffee-mode-hook-fn ()
  (paredit-c-mode)
  (setq coffee-tab-width 2))
(add-hook 'coffee-mode-hook 'dan/coffee-mode-hook-fn)

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
(add-hook 'minibuffer-inactive-mode-hook 'dan/minibuffer-setup-hook-fn)

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
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("a455366c5cdacebd8adaa99d50e37430b0170326e7640a688e9d9ad406e2edfd" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "6343f4d41b209fe8990e3c5f4d2040b359612ef9cd8682f1e1e2a836beba8107" "4780d7ce6e5491e2c1190082f7fe0f812707fc77455616ab6f8b38e796cbffa9" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "4e5e58e42f6f37920b95a8502f488928b3dab9b6cc03d864e38101ce36ecb968" "72759f4e42617df7a07d0a4f4b08982314aa97fbd495a5405c9b11f48bd6b839" "9e6ac467fa1e5eb09e2ac477f61c56b2e172815b4a6a43cf48def62f9d3e5bf9" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3b4800ea72984641068f45e8d1911405b910f1406b83650cbd747a831295c911" default)))
 '(default-input-method "latin-1-prefix")
 '(fci-rule-color "#383838")
 '(hl-sexp-background-color "#1c1f26")
 '(magit-diff-arguments (quote ("--ignore-all-space" "--no-ext-diff")))
 '(package-selected-packages
   (quote
    (docker magit-delta lsp-docker package-build flycheck-package xterm-color undercover simple-call-tree elisp-lint aio go-mode wdl-mode docker-tramp command-log-mode swift-mode ace-jump-mode ace-window applescript-mode auctex auctex-latexmk aumix-mode auto-overlays avy buffer-move cargo coffee-mode color-theme-modern color-theme-railscasts company-lean confluence counsel dash-functional debbugs dired-details+ dockerfile-mode dot-mode elisp-format emmet-mode eyuml f fill-column-indicator flycheck-rust fzf graphql-mode graphviz-dot-mode haskell-mode hindent htmlize ivy ivy-hydra jira-markup-mode latex-pretty-symbols lean-mode lsp-ui markdown-mode material-theme minimal-theme modalka multiple-cursors neotree paradox paredit paredit-everywhere plantuml-mode projectile py-isort railscasts-reloaded-theme railscasts-theme reformatter restclient ripgrep smartparens smooth-scroll soothe-theme sqlite sql-indent sublimity texfrag toml-mode transpose-frame typescript-mode use-package visual-fill-column wgrep yaml-mode yasnippet yasnippet-bundle zencoding-mode zones)))
 '(paradox-github-token t)
 '(safe-local-variable-values
   (quote
    ((checkdoc-minor-mode . 1)
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (org-src-preserve-indentation)
     (eval and
           (featurep
            (quote ox-extra))
           (ox-extras-activate
            (quote
             (ignore-headlines))))
     (eval require
           (quote ox-texinfo+)
           nil t)
     (eval require
           (quote ox-extra)
           nil t)
     (eval require
           (quote ol-man)
           nil t)
     (eval require
           (quote org-man)
           nil t)
     (eval require
           (quote magit-utils)
           nil t)
     (dan/python-project-root . "/Users/dan/src/elaenia/")
     (dan/python-project-name . "elaenia")
     (dan/python-virtualenv . "/Users/dan/tmp/virtualenvs/elaenia/")
     (dan/python-project-root . "/Users/dan/src/pytorch_examples")
     (dan/python-virtualenv . "/Users/dan/tmp/virtualenvs/misc")
     (dan/python-project-name . "pytorch_examples")
     (xenops-image-directory . "img")
     (bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(put 'upcase-region 'disabled nil)

(put 'LaTeX-narrow-to-environment 'disabled nil)
(message "⚡")
(put 'list-timers 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "dark cyan")))))
