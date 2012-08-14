(setq max-mini-window-height nil)
(add-to-list 'load-path "~/lib/emacs/python.el")
(require 'python)
(add-to-list 'load-path "~/lib/emacs/org/lisp")
(require 'org-install)
(require 'org)

(add-to-list 'load-path "~/projects/counsyl.el")
(require 'counsyl)
(defalias 'dan/find 'counsyl/grep)
(defalias 'dan/register-key-bindings 'counsyl/register-key-bindings)
(defalias 'dan--set-key-bindings 'counsyl--set-key-bindings)
(defvaralias 'dan/key-bindings 'counsyl/key-bindings)
(defun dan/python-import ()
  (interactive)
  (let ((import (counsyl/import)))
    (when import
      (python-shell-send-string-no-output import))))

(counsyl/register-key-bindings
 '(global-map .
              (("\C-c\M-g" . counsyl/grep))))
(require 'python)
(counsyl/register-key-bindings
 '("python" .
   (("\C-c\M-i" . counsyl/import)
    ([(super o)] . counsyl/open-in-github))))

(counsyl/register-key-bindings
 '("inferior-python" .
   (("\C-c\M-i" . counsyl/import))))


(setq org-babel-load-languages '((emacs-lisp . t) (R . t)))
(setq org-confirm-babel-evaluate nil)
(org-babel-load-file "~/config/emacs/emacs.org")
(load-file "~/config/emacs/extra.el")
