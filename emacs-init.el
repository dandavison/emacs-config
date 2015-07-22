(setq max-mini-window-height nil)
;; (add-to-list 'load-path "~/lib/emacs/python.el")
(require 'python)

(add-to-list 'load-path "~/clib/counsyl.el")
(require 'counsyl)
(defalias 'dan/register-key-bindings 'counsyl/register-key-bindings)
(defalias 'dan--set-key-bindings 'counsyl--set-key-bindings)
(defvaralias 'dan/key-bindings 'counsyl/key-bindings)
(defun dan/python-import ()
  (interactive)
  (let ((import (counsyl/python-import)))
    (when import
      (python-shell-send-string-no-output import))))

(defalias 'dan/find 'counsyl/ag)

;; (counsyl/register-key-bindings
;;  '(global-map .
;;               (("\C-c\M-f" . dan/find))))

(require 'python)
(counsyl/register-key-bindings
 '("python" .
   (("\C-c\M-i" . counsyl/python-import)
    ([(super o)] . counsyl/open-in-github))))

(counsyl/register-key-bindings
 '("inferior-python" .
   (("\C-c\M-i" . counsyl/python-import))))


(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
;; (org-babel-load-file "~/config/emacs/emacs.org")

(defun dan/tangle-emacs-dot-org ()
  (interactive)
  (shell-command
   "python ~/config/emacs/tangle.py < ~/config/emacs/emacs.org > ~/config/emacs/emacs.el"))

(dan/tangle-emacs-dot-org)
(load-file "~/config/emacs/emacs.el")
((lambda (file) (if (file-exists-p file) (load-file file))) "~/config/emacs/extra.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default-frame-alist (quote ((vertical-scroll-bars) (cursor-type . bar) (background-color . "white") (background-mode . light) (border-color . "black") (cursor-color . "black") (foreground-color . "black") (mouse-color . "black"))))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; '(default ((t (:inherit nil :stipple nil :background "#0a0a0a" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Menlo"))))
 '(comint-highlight-prompt ((t (:foreground "gold"))))
 ;; '(flymake-errline ((t (:background "dark blue" :foreground "black"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :foreground "black"))) t)
 '(org-block-end-line ((t (:inherit org-meta-line :foreground "black"))) t)
 '(org-document-info-keyword ((t (:inherit org-meta-line :foreground "gray93")))))
