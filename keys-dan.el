(setq dan/global-keybindings
      '(
	; ("\"" . dan/insert-double-quotes)
	; ("(" . insert-parentheses)
	; ("[" . dan/insert-square-brackets)
	; ("{" . dan/insert-curly-brackets)
	;; (global-set-key (kbd "[") #'insert-pair) use this strategy instead?
	("\C-n" . dan/next-line-and-indent)
	("\C-p" . dan/previous-line-and-indent)
	("\C-ca" . org-agenda)
	("\C-cf" . find-function)
	("\C-cg" . goto-line)
	("\C-ck" . dan/kill-line-and-indent)
	("\C-cm" . magit-status)
	("\C-cn" . dan/show-buffer-file-name)
	("\C-cr" . replace-regexp)
	("\C-\M-r" . ess-switch-to-end-of-ESS)
	("\C-cs" . search-forward-symbol-at-point)
	("\C-cv" . revert-buffer)
	("\C-\M-g" . lgrep)
	;; ("\C-c\C-r" . budget-eval)
	;; ("\C-c\C-w" . dan/xclip-kill)
	;; ("\C-c\C-y" . dan/xclip-yank)
	([(control next)] . end-of-buffer)
	([(control prior)] . beginning-of-buffer) 
	([(s tab)] . lisp-complete-symbol) 
	("\M-(" . dan/enclose-rest-of-line-in-parentheses)
	("\M-n" . forward-paragraph)
	("\M-p" . backward-paragraph)
	("\M-2" . dan/insert-double-quotes)
	([(meta left)] . winner-undo)
	([(meta right)] . winner-redo)
	([f1] . (lambda () (interactive) (switch-to-buffer "*Group*")))
	([f2] . dan/gnus-get-mail)
	([f3] . (lambda () (interactive) (switch-to-buffer "*Org Agenda*")))
	([f4] . (lambda () (interactive) (switch-to-buffer "*shell*")))
	([f5] . (lambda () (interactive) (switch-to-buffer "*Python*")))
	([f7] . dan/org-schedule-task-with-link)
	([f8] . org-remember)
	([f9] . find-tag-at-point)
	([f10] . delete-other-windows)
	([f11] . delete-window)
	([delete] . kill-buffer)
	([escape] . other-window)
	;; ("\C-q" . isearch-exit)
	;; ("\C-cf" . font-lock-fontify-buffer)
	;; ("\C-ci" . indent-buffer)
	))

(setq dan/ess-and-iess-keybindings
      '(
	;; ("\"" . dan/insert-double-quotes)
	;; ("[" . dan/insert-square-brackets)
	;; ("{" . dan/insert-curly-brackets)
	;; ("(" . dan/ess-insert-parentheses)
	;; ("-" . ess-smart-underscore)
	;; ("_" . dan/insert--)
	("\C-c?" . ess-display-help-on-object)
	("\C-ca" . ess-r-args-show)
	("\C-cd" . dan/ess-list-R-function-definitions)
	("\C-cf" . dan/ess-insert-function-template)
	("\C-ck" . dan/ess-kill-line-and-indent)
	("\C-cx" . dan/ess-recover-R-process)
	([(control return)] . ess-eval-line-and-step)
	([(shift tab)] . ess-complete-object-name)
        ;; ([tab] . comint-dynamic-complete)
	))

(setq dan/org-mode-keybindings
      '(
	([(control down)] . org-insert-heading)
	([(control left)] . org-metaleft)
	([(control right)] . org-metaright)
	([(meta left)] . winner-undo)
	([(meta right)] . winner-redo)
	("\C-ch" . hide-subtree)
	("\C-ct" . org-hide-block-toggle)
	("\C-ci" . dan/org-toggle-iimage-in-org)
	("\C-cz" . dan/org-babel-switch-to-code-with-session)
	([(control \')] . dan/org-hide-block-and-switch-to-code-buffer)))

(setq dan/emacs-lisp-mode-keybindings
      '(("\C-cd" . edebug-defun)))
(setq dan/c-c++-mode-keybindings
      '(
	;; ("(" . insert-parentheses)
	("\C-c\C-l" . dan/compile-and-switch-to-iESS)))

(setq dan/python-mode-keybindings
      '(()))

(setq dan/latex-mode-keybindings
      '(([C-tab] . TeX-complete-symbol)
	("\C-ct" . org-toggle-latex-org)
	("\C-c=" . reftex-toc-dan)))

(setq dan/paste-mode-keybindings
      '(
	("\"" . self-insert-command)
	("(" . self-insert-command)
	("[" . self-insert-command)
	("-" . self-insert-command)
	("_" . self-insert-command)
	))
