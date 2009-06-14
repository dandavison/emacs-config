; Org-mode settings
;; (setq load-path (cons "/usr/local/share/emacs/site-lisp" load-path))
(setq load-path (cons "/usr/local/src/org-mode/contrib/lisp" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq default-major-mode 'org-mode)

(setq mary-org-file "~/mary/jobs/jobs.org")
(setq mary-org-dir "~/mary/jobs")

;; agenda
(setq org-agenda-files (list mary-org-dir))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 60)
(setq org-agenda-remove-tags t)
(setq org-todo-keywords '((sequence "TODO" "STARTED" "|" "DONE" "DEFERRED"))) 
(setq org-deadline-warning-days 7)

(org-agenda-list)
(delete-other-windows)

;; remember
(org-remember-insinuate)
(setq org-default-notes-file mary-org-file)
(setq org-remember-templates
      '(
        ("job" ?j "* %?\n%^T\n%i" mary-org-file "jobs")
	("task" ?t "* TODO %?\n  %i" mary-org-file "tasks")
	))

(setq org-special-ctrl-a t)
(setq org-special-ctrl-e t)
(setq org-hide-leading-stars t)
(setq org-show-entry-below t)

(setq mary-org-mode-keybindings
      '(
	;; ("\C-cl" 'org-store-link)
	([(control down)] . org-insert-heading)
	([(control left)] . org-metaleft)
	([(control right)] . org-metaright)
	([(meta left)] . winner-undo)
	([(meta right)] . winner-redo)
	))

(defun mary-org-mode-hook ()
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	mary-org-mode-keybindings))

(add-hook 'org-mode-hook 'mary-org-mode-hook)
