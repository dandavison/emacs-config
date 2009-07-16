;;* Org-mode settings

(add-to-list 'load-path "/usr/local/src/org-mode/lisp")
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-to-list 'load-path "/usr/local/src/org-mode/contrib/lisp")
(require 'org-mairix)

(server-start)
;; (add-to-list 'load-path "~/path/to/org/protocol/")
(require 'org-protocol)

(require 'org-R)
(load "~/src/org/org-util.el")
(load "~/src/org/org-R/org-R.el")

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;* org-babel
(add-to-list 'load-path "~/src/org-babel/lisp")
(require 'org-babel-init)
(setq org-babel-tangle-langs
      '(
	("python" . ("py" "#!/usr/bin/env python"))
	("R" . ("R" "#!/usr/bin/env R"))
	("sh" . ("sh" "#!/usr/bin/env bash"))
	("emacs-lisp" . ("el" ""))))
			      
;;* export latex
(setq org-export-latex-classes nil)
(add-to-list 'org-export-latex-classes
	     '("beamer"
	       "\\documentclass[11pt]{beamer}\n\\usepackage[utf8]{inputenc}\n\\usepackage[T1]{fontenc}\n\\usepackage{hyperref}\n\\usepackage{verbatim}\n\\setbeameroption{show notes}\n\\usetheme{Goettingen}\n\\useoutertheme{infolines}"
	       ("\\begin{frame}\\frametitle{%s}\\begin{itemize}" "\\end{itemize}\\end{frame}"
		"\\begin{frame}\\frametitle{%s}\\begin{itemize}" "\\end{itemize}\\end{frame}")
	       ("\\item{%s}" . "\\item*{%s}")
	       ("\\end{itemize}\\note{" "}")))

(load "~/website/website.el")

;; (setq org-dan-expert t)
(defun dan-org-mode-hook ()
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	dan-org-mode-keybindings))
(add-hook 'org-mode-hook 'dan-org-mode-hook)


;; for color-theme-charcoal-black (can this be done programatically?)

(add-hook 'org-src-mode-hook '(lambda () (outline-minor-mode nil)))


;;* agenda
(org-defkey org-agenda-mode-map [(right)] 'forward-char)
(org-defkey org-agenda-mode-map [(left)] 'backward-char)

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
(setq org-agenda-ndays 30)
(setq org-agenda-compact-blocks t)
(setq org-deadline-warning-days 7)
;; (set-face-foreground 'org-agenda-date-weekend "red")
;; (setq org-agenda-remove-tags t) not sure why I had this

(setq org-agenda-custom-commands
      '(
	("W" "Search for work items in state" todo "x"
	 ((org-agenda-files '("~/org/work.org"
			      "~/org/wtccc2.org"
			      "~/org/pobi.org"
			      "~/org/shellfish.org"))))
	("T" "Search for tasks items in state" todo "x"
	 ((org-agenda-files '("~/org/tasks.org"))))
	("C" "Search for computing items in state" todo "x"
	 ((org-agenda-files '("~/org/computing.org"))))
	))

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


;;* remember
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


(defun org-dan-schedule-task-with-link (remember-target-char &optional arg)
  "Schedule a task with a link to current buffer.
   This uses org-remember. The task is scheduled for today, and
may use one of several remember targets"
  (interactive "cSelect remember target: [w]ork [t]asks [p]eople [c]omputing")
  (case remember-target-char
    (?w (kmacro-exec-ring-item 
	 (quote ([3 108 f8 ?w return 3 12 up return return 3 3] 0 "%d")) arg))
    (?t (kmacro-exec-ring-item 
	 (quote ([3 108 f8 ?t return 3 12 up return return 3 3] 0 "%d")) arg))
    (?c (kmacro-exec-ring-item 
	 (quote ([3 108 f8 ?c return 3 12 up return return 3 3] 0 "%d")) arg))
    (?p (kmacro-exec-ring-item 
	 (quote ([3 108 f8 ?p return 3 12 up return return 3 3] 0 "%d")) arg))))

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
;; (set-face-attribute 'org-hide nil :foreground "white")
(setq org-export-with-LaTeX-fragments t)

;;* refiling
;; http://doc.norang.ca/org-mode.html#Refiling

;; Use IDO for target completion
(setq org-completion-use-ido t)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

;; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path 'file)

;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc 
(setq org-outline-path-complete-in-steps t)


;;* Extra org functions

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

(defun org-dan-read-subtrees ()
  "Return subtrees as a list of strings"
  (let ((subtrees))
    (while (or (looking-at "^*") (outline-next-heading))
      (outline-mark-subtree)
      (setq subtrees (cons (buffer-substring (point) (mark)) subtrees))
      (goto-char (mark)))
    (nreverse subtrees)))

(defun org-dan-reverse-subtrees ()
  "Reverse the order of all subtrees.

Should start by setting restriction?
"
  (interactive)
  (beginning-of-line)
  (let ((subtrees (org-dan-read-subtrees)))
    (beginning-of-buffer)
    (delete-region (point) (mark))
    (insert (mapconcat 'identity (nreverse subtrees) "\n"))))
