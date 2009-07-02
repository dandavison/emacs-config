;; Org-mode settings

(add-to-list 'load-path "/usr/local/src/org-mode/lisp")
(require 'org-install)
(add-to-list 'load-path "/usr/local/src/org-mode/contrib/lisp")
(require 'org-mairix)

(server-start)
;; (add-to-list 'load-path "~/path/to/org/protocol/")
(require 'org-protocol)

(require 'org-R)
(load "~/src/org/org-util.el")
(load "~/src/org/org-R/org-R.el")

(add-to-list 'load-path "~/src/org-babel/lisp")
(require 'org-babel-init)
(setq org-babel-tangle-langs
      '(
	("python" . ("py" "#!/usr/bin/env python"))
	("R" . ("R" "#!/usr/bin/env R"))
	("sh" . ("sh" "#!/usr/bin/env bash"))
	))
			      


(load "~/website/website.el")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-dan-expert t)
(defun dan-org-mode-hook ()
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	dan-org-mode-keybindings))
(add-hook 'org-mode-hook 'dan-org-mode-hook)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; for color-theme-charcoal-black (can this be done programatically?)

(add-hook 'org-src-mode-hook '(lambda () (outline-minor-mode nil)))


;;;
;;; agenda
;;;;


(setq org-dan-todo-keyword "x")
(setq org-dan-started-keyword "s")
(setq org-dan-done-keyword "o")
(setq org-dan-cancelled-keyword "n")


(setq org-todo-keywords 
      '((sequence 
	 "x(x!@/!@)" "s(s!@/!@)" "|" "o(o!@/!@)" "n(n!@/!@)")))
(setq org-todo-keyword-faces
      '(
	("x" . (:foreground "red" :weight bold))
	("s" . (:foreground "darkorange" :weight bold))
	("o" . (:foreground "green" :weight bold))
	("n" . (:foreground "black" :weight bold))
	))
(setq org-edit-src-persistent-message nil)
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-directory "~/org")
(setq org-agenda-files (list org-directory))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 7)
(setq org-agenda-compact-blocks t)
(setq org-deadline-warning-days 7)
;; (set-face-foreground 'org-agenda-date-weekend "red")
;; (setq org-agenda-remove-tags t) not sure why I had this

(setq org-agenda-custom-commands
      '(
	("W" "Search for work items in state" todo "x"
	 ((org-agenda-files '("~/org/work.org"))))
	("T" "Search for tasks items in state" todo "x"
	 ((org-agenda-files '("~/org/tasks.org"))))
	("C" "Search for computing items in state" todo "x"
	 ((org-agenda-files '("~/org/computing.org"))))
	))
;;; remember
(org-remember-insinuate)
(setq org-default-notes-file "~/org/etc.org")
;; (setq org-remember-default-headline "top")
(setq org-remember-templates
      '(
	("work" ?w "* x %?\nSCHEDULED: %^T  %i" "~/org/work.org" 'top)
	("task" ?t "* x %?\nSCHEDULED: %^T\n  %i" "~/org/tasks.org" 'top)
        ("event" ?e "* %?\n%^T\n %i" "~/org/events.org" 'top)
	("computing" ?c "* x %?\n  %i" "~/org/computing.org" 'top)
	("notes" ?n "* %?\n  %i" "~/org/notes.org" 'top)
	("dbm" ?d "* x %?\n  %i" "~/org/dbm.org" 'top)
	("music" ?m "* %?\n %i" "~/org/music.org" 'top)
	("people" ?p "* x %?\nSCHEDULED: %^T\n  %i" "~/org/people.org" 'top)
	("info" ?i "* %?\n %i" "~/zzz/info.org" 'top)
	))

(fset 'org-dan-store-link-in-notes-dot-org-macro
      (lambda (&optional arg)
	"Keyboard macro."
	(interactive "p")
	(kmacro-exec-ring-item
	 (quote ([3 108 f8 110 3 12 up return return 3 3]
		 0 "%d")) arg)))

(fset 'org-dan-store-link-in-work-dot-org-macro
   (lambda (&optional arg)
     "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item 
      (quote ([3 108 f8 119 return 3 12 up return return 3 3] 0 "%d")) arg)))



(fset 'org-dan-store-link-in-work-dot-org-macro-2
   (lambda (&optional arg)
     "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item 
      (quote ([3 108 f8 119 return 3 12] 0 "%d")) arg)
     (previous-history-element)
     (kmacro-exec-ring-item 
      (quote ([return return 3 3] 0 "%d")) arg)))

(defun org-dan-store-link-in-work-dot-org ()
  (interactive)
  (org-store-link t)
  (org-remember nil ?w)    
  (newline) ;; date
  (org-insert-link nil buffer-file-name)    
  ;; (previous-history-element)    
  ;; (newline) ;; defaults to stored link?    
  ;; (newline) ;; select default link name    
  ;; (newline) ;; confirm link name    
  (org-remember-finalize)) 
    
(setq org-completion-use-ido t)    
(setq org-odd-levels-only t)    
(setq org-startup-folded t)    
(setq org-cycle-emulate-tab t)
(setq org-special-ctrl-a t)
(setq org-special-ctrl-e t)
(setq org-return-follows-link t)
(setq org-hide-leading-stars t)
;; (setq org-show-entry-below t) not sure why I had this
(set-face-attribute 'org-hide nil :foreground "gray15")
(setq org-export-with-LaTeX-fragments t)

;; personal alteration
;; This sort of thing should probably be maintained using git somehow
;; there's a FAQ on the org web page about that
(defun org-agenda-format-date-aligned-dan (date)
  "Dan's modified version of `org-agenda-format-date-aligned'.

Format a date string for display in the daily/weekly agenda, or
timeline.  This function makes sure that dates are aligned for
easy reading.
"
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week
		    (calendar-absolute-from-gregorian date)))
	 (weekyear (cond ((and (= month 1) (>= iso-week 52))
			  (1- year))
			 ((and (= month 12) (<= iso-week 1))
			  (1+ year))
			 (t year)))
	 (weekstring (if (= day-of-week 1)
			 (format " W%02d" iso-week)
		       "")))
;;;     (format "%-10s %2d %s %4d%s"
;;; 	    dayname day monthname year weekstring)
    
    (format "%s %2d %s"
	    (substring dayname 0 3) day (substring monthname 0 3))))

(setq org-agenda-format-date 'org-agenda-format-date-aligned-dan)



(defun org-insert-link-maybe ()
  "insert a file link depending on the context"
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
	  (when (re-search-backward "[[:space:]]" nil t) (forward-char 1)
		(looking-at "\\[?\\[?file:?")))
        (progn (replace-match "") (org-insert-link '(4)) t)
      nil)))

(add-hook 'org-tab-first-hook 'org-insert-link-maybe)
