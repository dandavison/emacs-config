;;; Init
(setq debug-on-error t)
(unless (equal emacs-version "27.0.50") (package-initialize))
(setq use-package-always-demand t)

;;; Native packages

(use-package emacs
  :bind (("C-M-\\" . dan/indent-region)
         ("C-b" . backward-sexp)
         ("C-c 1" . flycheck-mode)
         ("C-c C-c" . dan/save-even-if-not-modified)
         ("C-c C-l" . eval-buffer)
         ("C-c C-r" . magit-file-rename)
         ("C-c C-z" . python-shell-switch-to-shell)
         ("C-c M-d" . dan/magit-diff)
         ("C-c M-f" . search-files-thing-at-point)
         ("C-c b" . magit-blame)
         ("C-c c" . (lambda () (interactive) (magit-show-commit "HEAD")))
         ("C-c e" . outline-show-all)
         ("C-c f" . search-files-by-name)
         ("C-c g" . magit-status)
         ("C-c l" . linum-mode)
         ("C-c m" . mc/edit-lines)
         ("C-c o" . dan/scratch-buffer)
         ("C-c r" . replace-regexp)
         ("C-c s" . (lambda () (interactive) (shell-command-on-region (region-beginning) (region-end) "sort -V" nil 'replace)))
         ("C-c w" . dan/list-window-configurations)
         ("C-f" . forward-sexp)
         ("C-s" . swiper)
         ("C-x C-f" . dan/find-file)
         ("C-x b" . dan/switch-to-buffer)
         ("C-x d" . dan/dired-no-ask)
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
         ([f10] . dan/list-window-configurations)
         ([f11] . dan/find-dot-emacs)
         ([f12] . modalka-mode)
         ([(control down)] . scroll-up-command)
         ([(control up)] . scroll-down-command)
         ([(kp-delete)] . modalka-mode)
         ([(meta down)] . dan/transpose-line-down)
         ([(meta shift left)] . dan/indent-shift-left)
         ([(meta shift right)] . dan/indent-shift-right)
         ([(meta up)] . dan/transpose-line-up)
         ([(shift super left)] . ivy-resume)
         ([(super G)] . isearch-repeat-backward)
         ([(super down)] . (lambda () (interactive) (set-mark-command t)))
         ([(super d)] . dan/bookmark-set)
         ([(super k)] . dan/bookmark-set)
         ([(super left)] . winner-undo)
         ([(super l)] . bookmark-bmenu-list)
         ([(super return)] . dan/maximize)
         ([(super right)] . winner-redo)
         ([(super ?&)] . (lambda () (interactive) (let ((kill-buffer-query-functions nil)) (kill-buffer))))
         ([(super ?,)] . dan/goto-definition)
         ([(super ?.)] . dan/grep)
         ([(super ?\;)] . dan/show-buffer-file-name)
         ([(super ?\])] . fci-mode)
         ([(super \\)] . dan/indent-region)
         ([(super |)] . dan/shell-command-on-region-and-replace)))

(use-package bookmark
  :bind (:map bookmark-bmenu-mode-map
              ("\C-x\C-s" . bookmark-save)))

(use-package cl)

(use-package comint
  :bind (:map comint-mode-map
              ([(meta up)] . comint-previous-matching-input-from-input)
              ([(meta down)] . comint-next-matching-input-from-input)
              ([(control up)] . comint-previous-matching-input-from-input)
              ([(control down)] . comint-next-matching-input-from-input)
              ("\C-l" . dan/comint-clear-buffer)))

(use-package compilation
  :defer t
  :bind (:map compilation-mode-map
              ([(return)] . compile-goto-error)
              ("\C-cd" . dan/delete-matching-lines)
              ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (dan/iterm2-dwim)))))

(use-package dired
  :bind (:map dired-mode-map
              ([left] . dired-up-directory)
              ([right] . dired-find-file)))
(use-package dired-x)

(use-package elisp-mode
  :bind (:map emacs-lisp-mode-map
              ("\C-cd" . edebug-defun)
              ("\C-c," . find-function)
              ("\C-c\C-r" . (lambda () (interactive) (call-interactively 'eval-region) (deactivate-mark)))
              ([tab] . dan/company-indent-or-complete)
              ([(super x)] . eval-defun)))

(use-package org
  :bind (:map org-mode-map
              ([(shift left)] . windmove-left)
              ([(shift right)] . windmove-right)
              ([(shift up)] . windmove-up)
              ([(shift down)] . windmove-down)
              ([(meta left)] . backward-word)
              ([(meta right)] . forward-word)))

(use-package outline
  :bind (:map outline-minor-mode-map
              ([(control tab)] . org-cycle)
              ([(backtab)] . org-global-cycle)))

(use-package python
  :bind (:map python-mode-map
              ("\C-cd" . dan/python-insert-ipdb-set-trace)
              ("\C-c\C-c" . dan/save-even-if-not-modified)
              ([tab] . dan/company-indent-or-complete)
              ([(super ?')] . flycheck-mode)
              ([(super i)] . dan/python-where-am-i)
              ([(meta shift right)] . python-indent-shift-right)
              ([(meta shift left)] . python-indent-shift-left)
              ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (jedi:goto-definition)))))


(load-file "~/src/emacs-config/lib.el")


;;; External Packages
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(use-package clojure-mode
  :defer t
  :bind (:map clojure-mode-map
              ("\C-x\C-e" . inf-clojure-eval-last-sexp)
              ("\C-c\C-z" . inf-clojure)))

(use-package company)

(use-package ess
  :load-path "~/src/3p/ESS/lisp")

(use-package f)

(use-package facet
  :load-path "~/src/facet/emacs")

(use-package flycheck
  :load-path "~/src/3p/flycheck")

(use-package haskell
  :defer t
  :bind (:map haskell-mode-map
              ("'" . self-insert-command)))

(use-package ivy
  :config
  (setq ivy-fixed-height-minibuffer t
        ivy-height #xFFFFFFFF)
  (defun -dan/swiper-around-advice (orig-fun &rest args)
    (let ((ivy-height 20))
      (apply orig-fun args)))
  (advice-add 'swiper :around '-dan/swiper-around-advice)
  (add-hook 'counsel-grep-post-action-hook 'dan/on-jump-into-buffer))

(use-package js
  :defer t
  :bind (:map js-mode-map
              ("\C-cd" . (lambda () (interactive) (insert "debugger;")))))

(use-package latex
  :defer t
  :bind (:map latex-mode-map
              ("\C-c\C-c" . (lambda () (interactive)
                              (condition-case nil
                                  (dan/org-babel-execute-non-native-src-block)
                                (error nil))
                              (dan/save-even-if-not-modified)))
              ("\C-xni" . dan/latex-focus-insert-comment-delimiters)
              ("\C-xnf" . dan/latex-focus)
              ("\C-xnu" . dan/latex-unfocus)
              ("\C-c|" . dan/latex-set-builder-pipe)
              ("\C-c/" . dan/latex-frac-or-unfrac)
              ([(super b)] . dan/latex-bold)
              ([(super d)] . dan/latex-definition)
              ([(super i)] . dan/latex-italic)
              ([(super t)] . dan/latex-fixed-width)))

(use-package magit)

(use-package markdown-mode
  :bind (:map markdown-mode-map
              ("$" . dan/paired-dollar)
              ("\M-q" . fill-paragraph)
              ;; force file write to force pelican reload
              ("\C-X\C-s" . (lambda () (interactive) (set-buffer-modified-p t) (save-buffer)))
              ([(meta left)] . left-word)
              ([(meta right)] . right-word)))

(use-package mhtml-mode
  :defer t
  :bind (:map mhtml-mode-ma
              ("\C-c\C-c" . emmet-expand-line)))

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
  :defer t
  :load-path "~/src/3p/org-mode/contrib/lisp")

(use-package paredit
  :bind (:map paredit-mode-map
              ("\\" . nil)
              (";" . nil)
              ("\"" . paredit-c/doublequote)))

(use-package paredit-c
  :load-path "~/src/paredit-c")

(use-package penrose-modes
  :defer t
  :load-path "~/src/3p/penrose-modes")

(use-package projectile
  :bind-keymap
  ([(super p)] . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#")
        projectile-buffers-filter-function 'projectile-buffers-with-file
        projectile-use-git-grep t
        projectile-git-command "git ls-files -zc --exclude-standard"  ;; remove -o
        projectile-enable-caching t
        projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-modes "dired-mode"))

(use-package tla-mode
  :defer t
  :load-path "~/src/3p/tla-mode")

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
(color-theme 'railscasts-reloaded t)
;; (color-theme 'leuven t)
(minimal-mode)

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

;;; Server

(require 'server)
(unless (server-running-p) (server-start))


;;; Etc
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(setq vc-follow-symlinks t)
(setq save-silently t)
(setq auto-save-default nil)
(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)
(setq global-auto-revert-non-file-buffers t)
(setq async-shell-command-buffer 'rename-buffer)
(setq shell-command-default-error-buffer "*Shell Command Error*")
(setq scroll-conservatively 101)
(setq enable-recursive-minibuffers t)

(setq electric-indent-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)
(set-default 'tab-width 4)


(global-eldoc-mode nil)
(setq dired-auto-revert-buffer t)
(setq-default dired-omit-files-p t)
(setq dired-omit-size-limit nil)
(setq dired-omit-files "^\\.\\|__pycache__\\|\\.pyc")  ;; "^\\.?#\\|^\\.$\\|^\\.\\.$" "\\.log\\|\\.aux\\|\\.out"
(put 'dired-find-alternate-file 'disabled nil)

(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate (lambda () nil))

(dan/set-exec-path-from-shell)

(setq-default fill-column 80)

(use-package fill-column-indicator
  :config
  (setq fci-rule-column fill-column
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
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))


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
(setq erc-nick "dd7")
;;; Plantuml
(setq plantuml-jar-path "/usr/local/Cellar/plantuml/1.2018.11/libexec/plantuml.jar")


;;; Python
(use-package pyenv-mode
  ;; :init
  ;; (add-to-list 'exec-path "~/.pyenv/shims")
  :ensure t
  :config
  (pyenv-mode))

(use-package python-environment
  :config
  (setq python-environment-directory (expand-file-name "~/tmp/virtualenvs")))

(use-package jedi-core
  :ensure t
  :config
  (setq jedi:environment-root "emacs-jedi")
  (pyenv-mode-set "3.6.8")
  (setq jedi:server-args '("--log" "/tmp/jediepcserver.log"
                           "--log-level" "INFO"))
  (jedi:install-server))

(use-package flycheck)


(defun dan/multi-set-default (name-value-pairs)
  (mapcar
   (lambda (pair) (set-default (make-variable-buffer-local (car pair)) (eval (cdr pair))))
   name-value-pairs))


(defvar-local dan/python-virtualenv nil
  "Absolute path to python virtualenv")

(defun dan/python-get-virtualenv ()
  "Infer absolute path to python virtualenv for current buffer.

If the file is in a virtualenv, then return the absolute path to the virtualenv.
Otherwise, use `projectile-project-name' to construct the path to the virtualenv."
  (let* ((file-name (buffer-file-name))
         (virtualenv-name
          (if (string-prefix-p python-environment-directory file-name)
              (second (f-split (string-remove-prefix python-environment-directory file-name)))
            (projectile-project-name))))
    (and virtualenv-name
         (f-join python-environment-directory virtualenv-name))))

(defun dan/python-mode-hook-fn ()
  (interactive)

  (when nil
    (let* ((config
            '(
              ;; virtualenv
              (dan/python-virtualenv . (dan/python-get-virtualenv))

              ;; flycheck
              (flycheck-flake8-maximum-line-length . 99)
              (flycheck-highlighting-mode . 'lines)
              (flycheck-python-flake8-executable . (f-join dan/python-virtualenv "bin/flake8"))
              (flycheck-python-mypy-executable . (f-join dan/python-virtualenv "bin/mypy"))
              (flycheck-flake8rc . (f-join (projectile-project-root) "tox.ini"))
              (flycheck-python-mypy-ini . (f-join (projectile-project-root) "tox.ini"))

              ;; *.py
              (python-fill-docstring-style . 'django)

              ;; shell
              (python-shell-virtualenv-root . dan/python-virtualenv)
              (python-shell-interpreter . (f-join dan/python-virtualenv "bin/ipython"))
              (python-shell-interpreter-args . "-i"))))

      (dan/multi-set-default config)
      (set (make-variable-buffer-local 'dan/python-buffer-config-variables) (mapcar 'car config)))

    (assert (f-directory? dan/python-virtualenv) t)
    (assert (f-executable? flycheck-python-flake8-executable) t)
    (assert (f-executable? flycheck-python-mypy-executable) t)
    (assert (f-file? flycheck-flake8rc) t)
    (assert (f-file? flycheck-python-mypy-ini) t)
    (assert (f-directory? python-shell-virtualenv-root) t)
    (assert (f-executable? python-shell-interpreter) t)


    (setf (flycheck-checker-get 'python-flake8 'next-checkers) '((t . python-mypy)))
    (setf (flycheck-checker-get 'python-mypy 'next-checkers) nil)
    (flycheck-select-checker 'python-flake8))


  (company-mode)
  (jedi:setup)
  (add-to-list 'company-backends 'company-jedi)

  (setq fill-column 99)
  (set (make-variable-buffer-local 'fci-rule-column) fill-column)

  (eldoc-mode -1)
  (paredit-c-mode)
  (set (make-variable-buffer-local 'prettify-symbols-alist)
       '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))


(add-hook 'python-mode-hook 'dan/python-mode-hook-fn 'append)



(defun dan/python-show-buffer-config ()
  (interactive)
  (with-temp-buffer-window
   "*Python Buffer Config*"
   nil nil
   (princ
    (format
     "Python buffer-local config\n--------------------------\n\n%s\n"
     (mapconcat (lambda (sym) (format "%-40s %s" (symbol-name sym) (eval sym)))
                dan/python-buffer-config-variables
                "\n")))))



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


(require 'py-isort)
(setq py-isort-options
      '("--lines=9999999"
        "--force_single_line_imports"
        "--dont-skip=__init__.py"))

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

;;; Elpy
;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (elpy-enable)

;;; Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)


;;; Magit
(setq magit-save-repository-buffers nil)
;; (setq magit-revert-buffers nil)
(setq magit-push-always-verify nil)
(setq magit-revision-insert-related-refs nil)
(setq magit-log-arguments '("--graph" "--decorate" "-n20"))
(setq magit-status-sections-hook
      '(
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
      [?\C-c ?g ?d ?r ?m ?a ?s ?t ?e ?r ?. ?. ?. return])

;;; Yasnippet
(require 'yasnippet)
(define-key yas/keymap [tab] 'yas/next-field)
(yas/initialize)
(defun dan/yas-load ()
  (interactive)
  (yas/load-directory "/Users/dan/src/emacs-config/snippets"))
(dan/yas-load)


;;; Mode hooks
(setq pulse-iterations 20)
(defun dan/on-jump-into-buffer ()
  (delete-other-windows)
  (outline-show-all)
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
  (paredit-mode)
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

(add-hook 'compilation-finish-functions 'filter-results-clean-up-compilation-buffer)

(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (company-mode)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)"))
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

(defun dan/LaTeX-mode-hook-fn ()
  (interactive)
  (dan/setup-paired-characters)

  ;; The following DNW for \begin for some reason. Try evaluating
  ;; (set (make-local-variable 'outline-regexp) "\\(\\\\sub\\|\\\\section\\|\\\\begin\\)")
  ;; in the buffer instead
  (dan/set-up-outline-minor-mode "\\(\\\\sub\\|\\\\section\\|\\\\begin\\)")

  (dan/set-up-outline-minor-mode "\\\\section")
  (dan/watch-mathematics)
  (add-to-list 'LaTeX-item-list
               '("align" . dan/latex-insert-item-in-align-environment))
  (add-to-list 'LaTeX-item-list
               '("align*" . dan/latex-insert-item-in-align-environment))
  (local-set-key [(super v)] 'dan/latex-yank-clipboard-image-maybe))
(add-hook 'LaTeX-mode-hook 'dan/LaTeX-mode-hook-fn)

(defun dan/magit-diff-mode-hook-fn ()
  (dan/magit-hide-all-sections))
(add-hook 'magit-diff-mode-hook 'dan/magit-diff-mode-hook-fn)

(add-hook 'magit-diff-visit-file-hook 'dan/on-jump-into-buffer)

(defun dan/makefile-mode-hook-fn ()
  (paredit-c-mode))
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
  (scheme-mode))
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
  (setq sqlind-indentation-offsets-alist dan/sqlind-offsets-alist))


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
 '(agda2-highlight-level 'non-interactive)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   '("4e5e58e42f6f37920b95a8502f488928b3dab9b6cc03d864e38101ce36ecb968" "72759f4e42617df7a07d0a4f4b08982314aa97fbd495a5405c9b11f48bd6b839" "9e6ac467fa1e5eb09e2ac477f61c56b2e172815b4a6a43cf48def62f9d3e5bf9" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3b4800ea72984641068f45e8d1911405b910f1406b83650cbd747a831295c911" default))
 '(magit-diff-arguments '("--ignore-all-space" "--no-ext-diff"))
 '(package-selected-packages
   '(applescript-mode auctex auctex-latexmk aumix-mode auto-overlays avy buffer-move coffee-mode color-theme-modern color-theme-railscasts company company-jedi confluence counsel counsel-projectile debbugs dired-details+ dockerfile-mode dot-mode emmet-mode ess eyuml f fill-column-indicator flycheck fzf graphviz-dot-mode haskell-mode hindent htmlize ivy jedi-core jira-markup-mode latex-pretty-symbols magit markdown-mode minimal-theme modalka multiple-cursors paredit paredit-everywhere plantuml-mode pony-mode projectile pyenv-mode py-isort railscasts-reloaded-theme railscasts-theme ripgrep rust-mode smartparens smooth-scroll soothe-theme sqlite sql-indent sublimity transpose-frame use-package visual-fill-column yaml-mode yasnippet yasnippet-bundle zencoding-mode zones))
 '(safe-local-variable-values '((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bold ((t (:weight bold))))
 '(font-latex-math-face ((t (:foreground "red"))))
 '(font-latex-verbatim-face ((t (:inherit nil))))
 '(help-argument-name ((t (:inherit nil))))
 '(highlight ((t (:background "yellow" :foreground "black"))))
 '(minibuffer-prompt ((t (:foreground "#FFD798" :weight bold))))
 '(org-block ((t (:background "#FFFFFF" :foreground "#000088"))))
 '(org-block-begin-line ((t (:background "#FFFFFF" :foreground "lightgrey" :underline nil))))
 '(org-block-end-line ((t (:background "#FFFFFF" :foreground "lightgrey" :overline nil))))
 '(org-done ((t (:background "palegreen" :foreground "darkgrey" :box (:line-width 1 :color "grey") :weight normal))))
 '(org-level-1 ((t (:background "#FFFFFF" :foreground "#CC7733" :overline nil :weight bold :height 120))))
 '(org-level-2 ((t (:background "#FFFFFF" :foreground "dark red" :overline nil :weight bold :height 120))))
 '(org-todo ((t (:foreground "darkgrey" :box (:line-width 1 :color "grey") :weight normal)))))
(put 'upcase-region 'disabled nil)


(message "⚡")
