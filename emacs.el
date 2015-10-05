;;; Packages
(require 'cl)
(require 'dired-x)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(load-file "~/.emacs.d/elpa/color-theme-railscasts-0.0.2/color-theme-railscasts.el")

(add-to-list 'load-path "~/src/smartparens") (require 'smartparens-config)
(add-to-list 'load-path "~/src/projectile") (require 'projectile)

(add-to-list 'load-path "~/src/1p/minimal") (require 'minimal)
(add-to-list 'load-path "~/src/1p/paredit-c") (require 'paredit-c)

(load-file "~/src/1p/emacs-config/lib.el")


;;; Etc
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq truncate-lines t)
(setq vc-follow-symlinks t)
(setq async-shell-command-buffer 'rename-buffer)

(setq-default fill-column 79)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(setq auto-save-default nil)

(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)
(setq global-auto-revert-non-file-buffers t)
(setq dired-auto-revert-buffer t)
(setq-default dired-omit-files-p t)

(server-start)
(dan/set-exec-path-from-shell)
(dan/set-exec-path-from-shell "PYTHONPATH")

(setq fci-rule-column 79)
(setq fci-rule-color "#A5BAF1")

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(winner-mode t)


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


;;; Appearance
(setq ns-use-native-fullscreen nil)
(setq ring-bell-function (lambda nil nil))

(add-to-list 'load-path "~/src/1p/minimal")
(minimal-mode t)
(setq inhibit-startup-message t)
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")

(defun dan/set-appearance ()
  (interactive)
  (scroll-bar-mode -1)
  (set-cursor-color "red")
  (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (setq-default cursor-in-non-selected-windows nil)
  (setq cursor-type 'bar)
  (blink-cursor-mode -1)

  (set-face-background 'fringe (face-background 'default))
  (dan/set-show-paren-style)
  (font-lock-fontify-buffer))

(defun dan/set-show-paren-style ()
  (show-paren-mode t)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil :foreground "red"))

(dan/set-appearance)



;;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")


;;; Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)


;;; Magit
(setq magit-save-repository-buffers nil)
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

;;; Projectile
(setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#"))
;;; Yasnippet
(setq yas/trigger-key "\C-cy")
(define-key yas/keymap [tab] 'yas/next-field-group)
(yas/initialize)
(yas/load-directory (concat (file-name-directory (buffer-file-name)) "snippets"))

;;; Keys

(dan/register-key-bindings
 '(global-map
   .
   (("\C-b" . backward-sexp)
    ("\C-f" . forward-sexp)
    ("\C-xb" . dan/switch-to-buffer)
    ("\C-x\C-f" . dan/find-file)
    ("\C-xd" . dan/dired-no-ask)
    ("\C-ce" . show-all)
    ("\C-cg" . magit-status)
    ("\C-co" . dan/scratch-buffer)
    ("\C-cr" . replace-regexp)
    ("\M-i" . dan/highlight)
    ("\C-c\M-f" . dan/search)
    ("\C-x\C-c" . kill-emacs)
    ("\M-o" . dan/occur)
    ([f1] . other-window)
    ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
    ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
    ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
    ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
    ([(super i)] . fci-mode)
    ([(super left)] . winner-undo)
    ([(super right)] . winner-redo)
    ([(super return)] . delete-other-windows))))

(global-set-key (kbd "s-,") 'dan/show-buffer-file-name)

(dan/register-key-bindings
 '("comint" .
   (([(meta up)] . comint-previous-matching-input-from-input)
    ([(meta down)] . comint-next-matching-input-from-input)
    ([(control up)] . comint-previous-matching-input-from-input)
    ([(control down)] . comint-next-matching-input-from-input)
    ("\C-l" . dan/comint-clear-buffer))))

(dan/register-key-bindings
 '("emacs-lisp" .
   (("\C-cd" . edebug-defun)
    ("\C-c," . find-function)
    ([tab] . dan/indent-or-complete))))

(require 'markdown-mode)
(dan/register-key-bindings
 '("markdown" .
   (([(meta left)] . left-word)
    ([(meta right)] . right-word))))

(require 'python)
(dan/register-key-bindings
 '("python" .
   (("\C-cd" . dan/insert-ipdb-set-trace)
    ([(meta shift right)] . python-indent-shift-right)
    ([(meta shift left)] . python-indent-shift-left))))

(dan/register-key-bindings
 '(outline-minor-mode-map
   .
   (([(control tab)] . org-cycle)
    ([(backtab)] . org-global-cycle))))

(dan/register-key-bindings
 '("org"
   .
   (([(meta left)] . backward-word)
    ([(meta right)] . forward-word))))


;;; Mode hooks
(defun dan/on-jump-into-buffer ()
  (delete-other-windows)
  (show-all))

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)

(defun dan/before-save-hook-fn ()
  (dan/query-delete-trailing-whitespace))
(add-hook 'before-save-hook 'dan/before-save-hook-fn)

(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)"))
(add-hook 'emacs-lisp-mode-hook 'dan/emacs-lisp-mode-hook-fn)

(defun dan/html-mode-hook-fn ()
  (zencoding-mode))
(add-hook 'html-mode-hook 'dan/html-mode-hook-fn)

(defun dan/inferior-python-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'inferior-python-mode-hook 'dan/inferior-python-mode-hook-fn)

(defun dan/js-mode-hook-fn ()
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'dan/js-mode-hook-fn)

(defun dan/minibuffer-setup-hook-fn ()
  (when (eq this-command 'eval-expression)
    (setq completion-at-point-functions '(lisp-completion-at-point t))
    (local-set-key [tab] 'complete-symbol)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'dan/minibuffer-setup-hook-fn)

(defun dan/next-error-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'next-error-hook 'dan/next-error-hook-fn)

(defun dan/occur-mode-find-occurrence-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'occur-mode-find-occurrence-hook 'dan/occur-mode-find-occurrence-hook-fn)

(defun dan/python-mode-hook-fn ()
  (paredit-c-mode)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
(add-hook 'python-mode-hook 'dan/python-mode-hook-fn)
