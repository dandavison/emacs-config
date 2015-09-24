;;; Packages
(require 'cl)
(require 'dired-x)
(package-initialize)
(add-to-list 'load-path "~/src/1p/minimal") (require 'minimal)
(add-to-list 'load-path "~/src/1p/paredit-c") (require 'paredit-c)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


(load-file "~/src/1p/emacs-config/lib.el")
(load-file "~/src/1p/emacs-config/experimental.el")


;;; Etc
(setq make-backup-files nil)
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


;;; Search
(setq ag-arguments
      '("--line-number" "--smart-case" "--nogroup" "--column" "--"))
(setq dan/extra-ag-arguments
      '("--ignore" "*/migrations/*"
        "--ignore" "*/logs/*"
        "--ignore" "*/thirdparty/*"))

;;; Appearance



(load-file "~/.emacs.d/elpa/color-theme-railscasts-0.0.2/color-theme-railscasts.el")
(setq ns-use-native-fullscreen nil)
(setq ring-bell-function (lambda nil nil))

(add-to-list 'load-path "~/src/1p/minimal")
(minimal-mode t)
(setq inhibit-startup-message t)
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")

(defun dan/set-appearance ()
  (interactive)
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


;;; Windows
(winner-mode t)


;;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")


;;; Keys

(dan/register-key-bindings
 '(global-map
   .
   (("\C-b" . backward-sexp)
    ("\C-f" . forward-sexp)
    ("\C-xd" . dan/dired-no-ask)
    ("\C-ce" . show-all)
    ("\C-cg" . magit-status)
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
    ([(meta down)] . comint-next-matching-input-from-input))))

(dan/register-key-bindings
 '("emacs-lisp" .
   (("\C-cd" . edebug-defun)
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
(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)"))
(add-hook 'emacs-lisp-mode-hook 'dan/emacs-lisp-mode-hook-fn)

(defun dan/python-mode-hook-fn ()
  (paredit-c-mode)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
(add-hook 'python-mode-hook 'dan/python-mode-hook-fn)

(defun dan/inferior-python-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'inferior-python-mode-hook 'dan/inferior-python-mode-hook-fn)

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)

(defun dan/minibuffer-setup-hook-fn ()
  (when (eq this-command 'eval-expression)
    (setq completion-at-point-functions '(lisp-completion-at-point t))
    (local-set-key [tab] 'complete-symbol)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'dan/minibuffer-setup-hook-fn)

(defun dan/next-error-hook-fn ()
  (show-all))
(add-hook 'next-error-hook 'dan/next-error-hook-fn)

(defun dan/before-save-hook-fn ()
  (dan/query-delete-trailing-whitespace))
(add-hook 'before-save-hook 'dan/before-save-hook-fn)



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
(put 'downcase-region 'disabled nil)
