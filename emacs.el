;;; Packages
(require 'cl)
(require 'dired-x)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)

(add-to-list 'load-path "~/src/projectile") (require 'projectile)
(add-to-list 'load-path "~/src/magit/lisp") (require 'magit) ;; (require 'magit-autoloads)
(add-to-list 'load-path "~/src/ESS/lisp") (require 'ess)

(setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8029/plantuml.8029.jar")
(add-to-list 'load-path "~/src/puml-mode") (require 'puml-mode)

(add-to-list 'load-path "~/src/1p/minimal") (require 'minimal)
(add-to-list 'load-path "~/src/1p/paredit-c") (require 'paredit-c)
(add-to-list 'load-path "~/src/1p/emacs-search-files") (require 'search-files)

(load-file "~/src/1p/emacs-config/lib.el")


;;; Modes
(add-to-list 'auto-mode-alist '("\\.jira\\'" . jira-markup-mode))

;;; Server

(setq server-socket-dir "/tmp/emacs-sockets")
(make-directory server-socket-dir 'parents)
(set-file-modes server-socket-dir #o700)
(setq server-name (or (getenv "PROJECT_NAME") server-name))
(server-start)


;;; Etc
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(setq truncate-lines t)
(setq vc-follow-symlinks t)
(setq save-silently t)
(setq async-shell-command-buffer 'rename-buffer)

(setq-default fill-column 79)
(setq electric-indent-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(set-default 'tab-width 4)

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
(windmove-default-keybindings)

;;; Bookmarks
(setq bookmark-bmenu-file-column 80)
(setq bookmark-sort-flag nil)
(setq bookmark-save-flag 20)

;; message overwrites useful messages
(defun bookmark-maybe-message (fmt &rest args))


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
;; Doesn't respect ns-use-native-fullscreen if called now
;; (toggle-frame-fullscreen)

(setq ring-bell-function (lambda nil nil))

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
  (dan/set-show-paren-style))

(defun dan/set-show-paren-style ()
  (show-paren-mode t)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil :foreground "red"))

(minimal-mode)
(dan/set-appearance)


;;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(require 'py-isort)
(setq py-isort-options
      '("--lines=9999999"
        "--force_single_line_imports"
        "--dont-skip=__init__.py"))

(defvar dan/python-misc-dir "~/src/1p/misc")

;; (add-function :before (symbol-function 'run-python) 'dan/python-set-virtualenv)
;; (remove-function (symbol-function 'run-python) 'dan/python-set-virtualenv)

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



;;; Projectile
(setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#"))
(setq projectile-buffers-filter-function 'projectile-buffers-with-file)
(setq projectile-use-git-grep t)
(add-to-list 'projectile-project-root-files-functions
             'dan/projectile-root-by-parent-directory
             'append)
(add-to-list 'projectile-project-root-files-functions
             'dan/projectile-root-custom)

(add-to-list 'projectile-globally-ignored-modes "dired-mode")

;;; Helm
(add-to-list 'load-path "~/src/emacs-async")
(add-to-list 'load-path "~/src/helm")
(require 'helm-config)

(setq helm-grep-file-path-style 'relative)

(setq projectile-completion-system 'helm)
(helm-projectile-on)


;;; Yasnippet
(setq yas/trigger-key "\C-cy")
(define-key yas/keymap [tab] 'yas/next-field-group)
(yas/initialize)
(yas/load-directory "/Users/dan/src/dandavison--emacs-config/snippets")


;;; Keys

(dan/register-key-bindings
 '(global-map
   .
   (("\C-b" . backward-sexp)
    ("\C-f" . forward-sexp)
    ("\C-xb" . dan/switch-to-buffer)
    ("\C-x\C-f" . dan/find-file)
    ("\C-xd" . dan/dired-no-ask)
    ("\C-xp" . projectile-switch-project)
    ("\C-ce" . show-all)
    ("\C-cf" . search-files-by-name)
    ("\C-cg" . magit-status)
    ("\C-cl" . linum-mode)
    ("\C-co" . dan/scratch-buffer)
    ("\C-cr" . replace-regexp)
    ("\C-cw" . dan/list-window-configurations)
    ("\C-c\C-l" . eval-buffer)
    ("\C-c1" . flycheck-mode)
    ("\M-i" . dan/highlight)
    ("\C-c\M-f" . search-files-thing-at-point)
    ("\C-x\C-c" . kill-emacs)
    ("\C-z" . (lambda () (interactive)))
    ("\M-o" . dan/occur)
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
    ([f11] . (lambda (&optional arg) (interactive "P") (find-file (if arg "~/src/1p/emacs-config/lib.el" (file-chase-links "~/.emacs")))))
    ([f12] . dan/project-scratch-buffer)
    ([(meta shift left)] . dan/indent-shift-left)
    ([(meta shift right)] . dan/indent-shift-right)
    ([(super ?\])] . fci-mode)
    ([(super d)] . dan/bookmark-set)
    ([(super k)] . dan/bookmark-set)
    ;; ([(super k)] . (lambda (&optional arg) (interactive "P") (if arg (dan/bookmark-set) (dan/where-am-i))))
    ([(super l)] . bookmark-bmenu-list)
    ([(super ?,)] . search-files-read-from-minibuffer)
    ([(super ?.)] . search-files-thing-at-point)
    ([(super ?\;)] . dan/show-buffer-file-name)
    ([(super ?')] . dan/where-am-i)
    ([(super left)] . winner-undo)
    ([(super right)] . winner-redo)
    ([(super return)] . dan/maximize)
    ([(super |)] . dan/shell-command-on-region-and-replace))))

(global-set-key (kbd "s-,") 'dan/show-buffer-file-name)

(require 'bookmark)
(dan/register-key-bindings
 '("bookmark-bmenu" .
   (("\C-x\C-s" . bookmark-save))))

(require 'clojure-mode)
(dan/register-key-bindings
 '("clojure" .
   (("\C-x\C-e" . inf-clojure-eval-last-sexp)
    ("\C-c\C-z" . inf-clojure))))

(dan/register-key-bindings
 '("comint" .
   (([(meta up)] . comint-previous-matching-input-from-input)
    ([(meta down)] . comint-next-matching-input-from-input)
    ([(control up)] . comint-previous-matching-input-from-input)
    ([(control down)] . comint-next-matching-input-from-input)
    ("\C-l" . dan/comint-clear-buffer))))

(dan/register-key-bindings
 '("compilation" .
   (("\C-cd" . dan/delete-matching-lines))))


(dan/register-key-bindings
 '("emacs-lisp" .
   (("\C-cd" . edebug-defun)
    ("\C-c," . find-function)
    ([tab] . dan/indent-or-complete))))

(require 'js)
(dan/register-key-bindings
 '("js" .
   (("\C-cd" . (lambda () (interactive) (insert "debugger;"))))))

(require 'markdown-mode)
(dan/register-key-bindings
 '("markdown" .
   (([(meta left)] . left-word)
    ([(meta right)] . right-word))))


(require 'org)
(dan/register-key-bindings
 '("org" .
   (([(shift left)] . windmove-left)
    ([(shift right)] . windmove-right))))


(require 'python)
(dan/register-key-bindings
 '("python" .
   (("\C-cd" . dan/insert-ipdb-set-trace)
    ([(super i)] . dan/python-where-am-i)
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
  (show-all)
  (when (eq major-mode 'python-mode)
    (dan/python-current-defun-name)))

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)

(defun dan/before-save-hook-fn ()
  (dan/query-delete-trailing-whitespace))
(add-hook 'before-save-hook 'dan/before-save-hook-fn)

(defun dan/clojure-mode-hook-fn ()
  (paredit-mode)
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

(defun dan/compilation-finish-fn ()
  (dan/clean-up-compilation-buffer))
(add-hook 'compilation-finish-functions 'dan/compilation-finish-fn)

(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)"))
(add-hook 'emacs-lisp-mode-hook 'dan/emacs-lisp-mode-hook-fn)

(defun dan/eshell-mode-hook-fn ()
  (paredit-mode t)
  (dan/pretty-lambdas))
(add-hook 'eshell-mode-hook 'dan/eshell-mode-hook-fn)

(defun dan/r-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'ess-mode-hook 'dan/r-mode-hook-fn)
(add-hook 'inferior-ess-mode-hook 'dan/r-mode-hook-fn)

(defun dan/find-function-after-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'find-function-after-hook 'dan/find-function-after-hook-fn)

(defun dan/html-mode-hook-fn ()
  (zencoding-mode))
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

(defun dan/makefile-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'makefile-mode-hook 'dan/makefile-mode-hook-fn)

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


;; (advice-add 'revert-buffer :around (symbol-function 'save-excursion))
(defun dan/python-mode-hook-fn ()
  (paredit-c-mode)
  (dan/pretty-lambdas)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
(add-hook 'python-mode-hook 'dan/python-mode-hook-fn)
(put 'dired-find-alternate-file 'disabled nil)

(defun dan/sh-mode-hook-fn ()
  (setq sh-indentation 4)
  (setq sh-basic-offset)
  (paredit-c-mode))
(add-hook 'sh-mode-hook 'dan/sh-mode-hook-fn)


;;; Spam

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3b4800ea72984641068f45e8d1911405b910f1406b83650cbd747a831295c911" default)))
 '(package-selected-packages
   (quote
    (color-theme-modern zones py-isort jira-markup-mode flycheck-package inf-clojure auto-overlays aumix-mode plantuml-mode buffer-move confluence ess zencoding-mode yasnippet-bundle yasnippet yaml-mode smartparens rust-mode railscasts-theme paredit-everywhere minimal-theme markdown-mode latex-pretty-symbols flycheck flx-ido fill-column-indicator eyuml evil dockerfile-mode dired-details+ color-theme-railscasts coffee-mode clojure-mode auctex ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
