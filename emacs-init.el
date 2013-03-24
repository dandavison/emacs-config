(setq max-mini-window-height nil)
(add-to-list 'load-path "~/lib/emacs/python.el")
(require 'python)
;; (add-to-list 'load-path "~/lib/emacs/org/lisp")
;; (require 'org-install)
;; (require 'org)

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

(defun dan/find ()
  (interactive)
  (call-interactively 'counsyl/grep)
  (other-buffer))

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
(shell-command "python ~/config/emacs/tangle.py < ~/config/emacs/emacs.org > ~/config/emacs/emacs.el")
(load-file "~/config/emacs/emacs.el")
(load-file "~/config/emacs/extra.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(comint-highlight-prompt ((t (:foreground "gold"))))
 '(org-block-begin-line ((t (:inherit org-meta-line :foreground "gray93"))) t)
 '(org-block-end-line ((t (:inherit org-meta-line :foreground "gray93"))) t)
 '(org-document-info-keyword ((t (:inherit org-meta-line :foreground "gray93")))))
