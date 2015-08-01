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

(setq-default indent-tabs-mode nil)

(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)
(setq global-auto-revert-non-file-buffers t)
(setq dired-auto-revert-buffer t)

(server-start)
(dan/set-exec-path-from-shell)
(dan/set-exec-path-from-shell "PYTHONPATH")


;;; Packages
(package-initialize)
(add-to-list 'load-path "~/src/1p/minimal") (require 'minimal)
(add-to-list 'load-path "~/src/1p/paredit-c") (require 'paredit-c)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


;;; Appearance
;; (load-file "~/.emacs.d/elpa/color-theme-railscasts-0.0.2/color-theme-railscasts.el")
(setq ns-use-native-fullscreen nil)
(setq ring-bell-function (lambda nil nil))

(add-to-list 'load-path "~/src/1p/minimal")
(minimal-mode t)
(setq inhibit-startup-message t)
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")


(dan/set-appearance)




;;; Windows
(winner-mode t)

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
    ("\M-o" . dan/occur)	       
    ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
    ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
    ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
    ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
    ([(control return)] . delete-other-windows)
    ([(control left)] . winner-undo)
    ([(control right)] . winner-redo)
    ([(super return)] . toggle-frame-fullscreen))))

(global-set-key (kbd "s-,") 'dan/show-buffer-file-name)


(dan/register-key-bindings
 '("emacs-lisp" .
   (([tab] . dan/indent-or-complete))))

(require 'markdown-mode)
(dan/register-key-bindings
 '("markdown" .
   (([(meta left)] . left-word)
    ([(meta right)] . right-word))))

(require 'python)
(dan/register-key-bindings
 '("python" .
   (("\C-cd" . dan/insert-ipdb-set-trace))))


(dan/register-key-bindings
 '(outline-minor-mode-map
   .
   (([(control tab)] . org-cycle)
    ([(backtab)] org-global-cycle))))


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
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
(add-hook 'python-mode-hook 'dan/python-mode-hook-fn)

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)
;; (add-hook 'find-file-hook 'dan/set-appearance)


;;; Magit
(setq magit-save-repository-buffers nil)
