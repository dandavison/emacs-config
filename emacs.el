(load-file "~/src/1p/emacs-config/lib.el")


;;; Etc
(global-auto-revert-mode t)
(setq auto-revert-interval 1)
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
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")


(dan/set-appearance)




;;; Windows
(winner-mode t)

;;; Keys

(dan/register-key-bindings
 '(global-map .
              (("\C-b" . backward-sexp)
               ("\C-f" . forward-sexp)
               ("\C-xd" . dan/dired-no-ask)
               ("\C-cg" . magit-status)
               ("\M-o" . dan/occur)	       
               ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
               ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
               ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
               ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
               ([(control return)] . delete-other-windows)
	       ([(control left)] . winner-undo)
               ([(control right)] . winner-redo)
               ([(super return)] . toggle-frame-fullscreen))))

(dan/register-key-bindings
 '("emacs-lisp" .
   (([tab] . dan/indent-or-complete))))

(dan/register-key-bindings
 '("markdown" .
   (([(meta left)] . left-word)
    ([(meta right)] . right-word))))

(require 'python)
(dan/register-key-bindings
 '("python" .
   (("\C-cd" . dan/insert-ipdb-set-trace))))


;;; Mode hooks
(add-hook 'find-file-hook 'dan/set-appearance)
(add-hook 'emacs-lisp-mode-hook 'dan/pretty-lambdas)
(add-hook 'python-mode-hook
	  (lambda () (paredit-c-mode)))
(add-hook 'after-change-major-mode-hook
	  (lambda () (dan/set-appearance)))


;;; Magit
(setq magit-save-repository-buffers nil)
