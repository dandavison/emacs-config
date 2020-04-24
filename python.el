(if nil
    (use-package jedi-core
      :load-path "~/src/3p/emacs-jedi"
      :config
      (setq jedi:server-args '("--log" "/tmp/jediepcserver.log"
                               "--log-level" "INFO"
                               "--log-traceback"))
      (setq
       jedi:goto-definition-config
       '((nil definition nil)
         (t   definition nil)
         (nil nil        nil)
         (t   nil        nil)
         (nil definition t  )
         (t   definition t  )
         (nil nil        t  )
         (t   nil        t  )))
      ;; :hook
      ;; (jedi:goto-definition-hook . #'dan/on-jump-into-buffer)
      )

  (use-package company-jedi
    :load-path "~/src/3p/company-jedi"
    :after company
    :config
    (add-to-list 'company-backends 'company-jedi)))

(defvar dan/virtualenvs-directory (expand-file-name "~/tmp/virtualenvs"))

(defvar-local dan/python-project-name nil
  "The name of the current python project.

Suppose the name is $name. The following statements are true:
1. The project is held in a git repository at a path like .../$name/.git.
2. The full path is known to projectile.
3. The project virtualenv is a directory also named $name. Its
   parent directory is dan/virtualenvs-directory.")

(defvar-local dan/python-virtualenv nil
  "Absolute path to python virtualenv for current buffer.")


(defvar-local dan/python-project-root nil
  "Absolute path to project root.
The project root is the place where you might find tox.ini, setup.py, Makefile, etc.")


(defun dan/python-mode-hook-fn ()
  (interactive)

  (if nil
      (progn
        (setq-local dan/python-virtualenv  "/Users/dan/tmp/virtualenvs/elaenia/")
        (setq-local  dan/python-project-name "elaenia")
        (setq-local dan/python-project-root "/Users/dan/src/elaenia/")))

  (unless dan/python-project-name
    (setq-local dan/python-project-name (dan/python-infer-project-name)))
  (unless dan/python-project-root
    (setq-local dan/python-project-root (dan/python-infer-project-root dan/python-project-name)))
  (unless dan/python-virtualenv
    (setq-local dan/python-virtualenv (dan/python-infer-virtualenv dan/python-project-name)))

  (if (and dan/python-virtualenv
           dan/python-project-root)
      (progn
        (let* ((config
                '(;; Project
                  (dan/python-project-name . dan/python-project-name)
                  (dan/python-virtualenv . dan/python-virtualenv)
                  (dan/python-project-root . dan/python-project-root)

                  ;; Flycheck
                  (flycheck-flake8-maximum-line-length . 99)
                  (flycheck-highlighting-mode . 'lines)
                  (flycheck-python-flake8-executable . (f-join dan/python-virtualenv "bin/flake8"))
                  (flycheck-python-mypy-executable . (f-join dan/python-virtualenv "bin/mypy"))
                  (flycheck-flake8rc . (f-join dan/python-project-root "tox.ini"))
                  (flycheck-python-mypy-ini . (f-join dan/python-project-root "tox.ini"))

                  ;; Jedi
                  (jedi:virtualenv . dan/python-virtualenv)

                  ;; Shell
                  (python-shell-virtualenv-root . dan/python-virtualenv)
                  (python-shell-interpreter . (f-join dan/python-virtualenv "bin/ipython"))
                  (python-shell-interpreter-args . "-i"))))

          (mapcar (lambda (pair) (set (make-variable-buffer-local (car pair)) (eval (cdr pair))))
                  config)
          (set (make-variable-buffer-local 'dan/python-buffer-config-keys)
               (mapcar 'car config)))

        ;; (assert (f-directory? dan/python-virtualenv) t)
        ;; (assert (f-directory? dan/python-project-root) t)
        ;; (assert (f-executable? flycheck-python-flake8-executable) t)
        ;; (assert (f-executable? flycheck-python-mypy-executable) t)
        ;; (assert (f-file? flycheck-flake8rc) t)
        ;; (assert (f-file? flycheck-python-mypy-ini) t)
        ;; (assert (f-directory? python-shell-virtualenv-root) t)
        ;; (assert (f-executable? python-shell-interpreter) t)

        ;; (setf (flycheck-checker-get 'python-flake8 'next-checkers) '((t . python-mypy)))
        (put 'python-flake8 (flycheck--checker-property-name 'next-checkers) '((t . python-mypy)))
        ;; (setf (flycheck-checker-get 'python-mypy 'next-checkers) nil)
        (put 'python-mypy (flycheck--checker-property-name 'next-checkers) nil)

        ;; (condition-case error
        ;;     (flycheck-select-checker 'python-flake8)
        ;;   (error (progn
        ;;            (flycheck-mode -1)
        ;;            (message "dan/python-mode-hook-fn: Error thrown by (flycheck-select-checker 'python-flake8). Disabling flycheck: %S" error))))

        ;; (blacken-on-save-mode)
        ;; (add-hook 'before-save-hook #'dan/python-blacken-defun-on-save nil t)
        ;; (add-hook 'before-save-hook (lambda () (blacken-buffer t)) nil t)
        ;; (flycheck-mode -1)
        )
    (message "dan/python-mode-hook-fn: Python virtualenv / project root are unknown"))

  (company-mode)
  ;; (eglot)

  (setq fill-column 99)
  (set (make-variable-buffer-local 'fci-rule-column) fill-column)

  (setq python-fill-docstring-style 'django)
  (paredit-c-mode)
  (set (make-variable-buffer-local 'prettify-symbols-alist)
       '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
