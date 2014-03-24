(require 'cl)

(defvar dan/operating-system
  (intern (downcase
           (replace-regexp-in-string
            "\n" ""
            (shell-command-to-string "uname"))))
  "The current OS")

(setq dan/starter-kit-components-all '(bindings defuns eshell lisp misc org perl registers ruby yasnippet))
(setq dan/starter-kit-components '())

(let ((dotfiles-dir "~/lib/emacs/emacs-starter-kit"))
  (mapcar
   'org-babel-load-file
   (mapcar
    (lambda (x) (format "%s/starter-kit-%s.org" dotfiles-dir (symbol-name x)))
    dan/starter-kit-components)))

(defun esk/pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun dan/filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun dan/dedent (text)
  (with-temp-buffer
    (insert text)
    (org-do-remove-indentation 999)
    (buffer-string)))

(defun dan/indent (text column)
  (with-temp-buffer
    (insert text)
    (let ((indent-tabs-mode nil))
      (indent-region (point-min) (point-max) column))
    (buffer-string)))



(defun dan/shell-command-on-file (cmd)
  (interactive "sCommand: ")
  (shell-command (format "%s %s" cmd (buffer-file-name))))

(defun dan/indent-region (start end)
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (re-search-forward "^" nil t)
      (indent-for-tab-command))
    (widen)))

(defvar dan/after-save-command nil
  "This string will be executed as a shell command after saving
  the buffer.")

(defun dan/do-after-save-command ()
  (when dan/after-save-command
    (message dan/after-save-command)
    (save-window-excursion
      (async-shell-command dan/after-save-command))))

(add-hook 'after-save-hook 'dan/do-after-save-command)

(defun dan/set-after-save-command (cmd)
  (interactive "sCommand: ")
  (set (make-local-variable 'dan/after-save-command) cmd))

(defun dan/sync (&optional seq)
  (interactive "P")
  (let* ((remote (if seq "seq-analysis:seq_pipeline" "testp-dan:website"))
         (from (file-name-nondirectory (buffer-file-name)))
         (to (format "%s/%s" remote (counsyl--git-relative-path))))
    (dan/set-after-save-command
     (read-from-minibuffer
      "Sync command: "
      (format "rsync -u %s %s" from to)))))

(defun dan/dontfuckingdothat ()
  (interactive)
  (message "Don't fucking do that."))

(defun dan/paint-buffer-by-predicate (predicate)
  "Set background color at position x according to value of predicate."
  (interactive)
  (font-lock-mode -1)
  (let ((color "#4b89d0"))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (funcall predicate)
            (put-text-property
             (point) (1+ (point))
             'face
             `(:background ,color)))
        (forward-char 1)
        (if (eq (mod (point) 1000) 0)
            (message "%d" (point)))))))

(defun dan/open-github-path-from-clipboard ()
  (interactive)
  (with-temp-buffer
    (save-excursion (clipboard-yank))
    (when (looking-at
           (concat
            "https://github\..+\.com"
            "/dev/\\([^/]+\\)/"  ;; repo
            "\\(blob\\|tree\\)/"
            "\\([^/]+\\)/"       ;; branch
            "\\(.+\\)"))         ;; path
      (let ((repo (match-string 1))
            (branch (match-string 3))
            (path (match-string 4)))
        (find-file (format "~/%s/%s" repo path))))
    (message (buffer-string))))

(defun dan/delete-matching-lines (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'delete-non-matching-lines 'delete-matching-lines)))

(defun dan/toggle-read-only (&optional arg)
  (interactive "P")
  (unless arg (toggle-read-only))
  (dan/set-mode-line-background))

(defvar dan/mode-line-force-color-update-hack nil)

(defun dan/set-mode-line-background ()
  (setq minimal-mode-line-background
        (if buffer-read-only "darkred" "sea green"))
  (if dan/mode-line-force-color-update-hack
      ;; silly hack to make the modeline update
      (save-window-excursion
        (describe-variable 'minimal-mode-line-background)
        (message ""))))

  ;; (redraw-modeline t)
  ;; (redraw-frame (window-frame (selected-window)))
  ;; (redraw-modeline t)

(defvar dan/read-only-buffers-p nil
  "Automatically set buffers to read-only in find-file hook?")

(defvar dan/no-read-only-buffers
  "Names of buffers that should not be opened in read-only mode"
  '("git-rebase-todo" "COMMIT_EDITMSG"))

(defun dan/no-auto-read-only-p (buffer)
  (cond
   ((member (buffer-name buffer) dan/no-read-only-buffers))
   ((eq (with-current-buffer buffer major-mode) 'dired-mode))
   ((string-match "^scratch\." (buffer-name buffer)))
   (t nil)))

(defun dan/set-read-only-maybe ()
  (when dan/read-only-buffers-p
    (unless (dan/no-auto-read-only-p (current-buffer))
        (dan/toggle-read-only))))

(add-hook 'find-file-hook 'dan/set-read-only-maybe)
(add-hook 'find-file-hook 
          (lambda () (set (make-local-variable 'indent-tabs-mode) nil)))

(defun dan/indent-or-complete ()
  (interactive)
  (if (and (looking-at "[ \n)]")
           (looking-back "[^ \n]"))
      (complete-symbol nil)
    (indent-for-tab-command)))

(defun dan/codequality (&optional dir)
  (interactive "DBase Directory: ")
  (compile
   (format
    "find %s -type f -name '*.py' | grep -Fv '/migrations/'  | xargs codequality '{}' \;" dir dir)))

(defun dan/org-screenshot ()
"Store a screenshot in a subdirectory named screenshots and
insert a link in the current file."
  (interactive)
  ((lambda (file)
     (make-directory (file-name-directory file) t)
     (shell-command (format "screencapture -s %s" file))
     (insert (concat "[[file:" file "]]")))
  (format "screenshots/%s.png"
          (make-temp-name
           (file-name-nondirectory (buffer-file-name))))))

(defun dan/keys-enforce-global-bindings ()
  "Enforce global bindings in all buffers"
  (interactive)
  (mapc
   (lambda (map) (dan--set-key-bindings 'global-map map))
   (delete-dups
    (mapcar (lambda (buf) (with-current-buffer buf (current-local-map)))
            (buffer-list))))
  nil)

(defmacro dan/do-until-no-error (rest &rest body)
  "Repeat evaluation of BODY until no error.
Wait for REST between each attempt."
  `(while (null (ignore-errors (progn ,@body t))) (sit-for rest)))

(defun dan/occur-mode-goto-occurrence ()
  (interactive)
  (call-interactively 'occur-mode-goto-occurrence)
  (other-window 1))

(defun dan/occur ()
  (interactive)
  (let ((tap (thing-at-point 'symbol)))
    (and tap (push tap regexp-history)))
  (call-interactively 'occur)
  (let ((results-buffer (get-buffer "*Occur*")))
    (if results-buffer
        (progn
          (other-window 1)
          (let ((buffer-read-only)) (kill-line 1))
          (delete-other-windows))
      (message "No matches"))))


(defvar dan/highlighted nil)

(defun dan/highlight (&optional arg)
  "Highlight the word at point.

   With prefix arg, read the word to be highlighted from the
   minibuffer. To remove highlighting, call the function with no
   word at point."
  (interactive "P")
  (let ((to-be-highlighted
         (if arg (read-from-minibuffer "Highlight: ")
           (thing-at-point 'symbol))))
    (if (equal to-be-highlighted dan/highlighted)
        (setq to-be-highlighted nil))
    (if to-be-highlighted
        (highlight-regexp to-be-highlighted 'magit-diff-del)
      (unhighlight-regexp dan/highlighted))
    (setq dan/highlighted to-be-highlighted)))

(defun dan/highlight-occur ()
  (interactive)
  (occur dan/highlighted))

(defun dan/occur-def-class ()
  (interactive)
  (push "^\\(class\\|def\\) " regexp-history)
  (call-interactively 'occur)
  (let ((results-buffer (get-buffer "*Occur*")))
    (if results-buffer
        (progn
          (other-window 1)
          (let ((buffer-read-only)) (kill-line 1)))
      (message "No matches"))))

;; (local-set-key [(return)] 'dan/occur-mode-goto-occurrence)


(defun dan/set-local-variables (alist)
  (dolist (pair alist)
    (set (make-local-variable (car (pair))) (cadr pair))))

(defun dan/save-value-to-kill-ring (&optional sexp)
  (interactive "XExpression to evaluate and save to kill-ring: ")
  (with-temp-buffer
    (let ((string (format "%s" sexp)))
      (insert string)
      (kill-ring-save (point-min) (point-max))
      string)))

(defun dan/save-buffer-file-name-to-kill-ring ()
  (interactive)
  (dan/save-value-to-kill-ring buffer-file-name))

(defun dan/sanitise-faces ()
  (interactive)
  ;; (set-face-background 'region (face-background 'default)) ;; don't highlight region
  (set-face-background 'fringe (face-background 'default)) ;; don't have different color fringe

  (set-face-background 'highlight (face-background 'default))
  (set-face-foreground 'highlight (face-foreground 'font-lock-comment-face))

  ;; (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (set-cursor-color "red")
  (set-face-attribute 'org-hide nil
                      :foreground
                      (face-attribute 'default :background))
  (dan/set-show-paren-style)
  (font-lock-fontify-buffer))

(defun dan/set-show-paren-style ()
  (interactive)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  ;; use these in a mode hook function
  ;; (make-variable-buffer-local 'show-paren-mode)
  ;; (show-paren-mode t)
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (set-face-attribute 'show-paren-match-face nil :foreground "red"))

(defun dan/set-exec-path-from-shell (&optional pathvar)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (replace-regexp-in-string
           "[[:space:]\n]*$" ""
           (shell-command-to-string
            (format "zsh -c '. ~/.zshrc 2>&/dev/null && echo $%s'" pathvar)))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))
(dan/set-exec-path-from-shell)
(dan/set-exec-path-from-shell "PYTHONPATH")
(setenv "GEM_HOME" (org-babel-chomp (shell-command-to-string "brew --prefix")))

(defun dan/sudo-read-from-file (file)
  (interactive "FFile: ")
  (with-temp-buffer
    (insert-file-contents
     (concat "/sudo::" (expand-file-name file)))
    (buffer-string)))

(defun dan/trace-functions (&optional regexp)
  "Trace functions with names matching regexp"
  ;; TODO: read regexp from minibuffer
  (interactive "sTrace functions matching: ")
  (mapc (lambda (sym) (message "Tracing %s" (symbol-name sym)) (trace-function sym)) 
        (loop for x being the symbols
              if (and (fboundp x) (string-match regexp (symbol-name x)))
              collect x)))

(defun dan/toggle-debug-on-error ()
  (interactive)
  (message
   "debug-on-error %s"
   (if (setq debug-on-error (not debug-on-error))
       "on" "off")))

(defun dan/revert-elisp-buffers ()
  "Revert all elisp buffers"
  (interactive)
  (save-excursion
    (dolist (buf (buffer-list))
      (set-buffer buf)
      (if (eq major-mode 'emacs-lisp-mode)
          (revert-buffer)))))

(defun dan/looking-at-string (string)
  (interactive)
  (string-equal
   string
   (buffer-substring-no-properties (point) (+ (point) (length string)))))

;; this doesn't write anything in minibuffer...
(defun dan/show-current-font()
  (interactive)
  (frame-parameter nil 'font))

;; Why doesn't this work? (Says something about wrong number of arguments)
(defun dan/indent-buffer ()
  "Indent whole buffer"
  (interactive)
  (mark-whole-buffer)
  (indent-region))

(defun dan/eval-buffer-confirm ()
  (interactive)
  (save-buffer)
  (eval-buffer)
  (message "loaded buffer %s" (buffer-name)))

;; http://blog.printf.net/ find-tag-at-point I often work on the
;; kernel or Xorg, and I would be totally ridiculously lost with both
;; if I wasn't using "tags" support in my editor. Here's how it works:
;; you run etags over your .[ch] files (or make tags in a kernel
;; source dir), and it generates a TAGS index. You load that in emacs
;; with M-x visit-tags-table, and with the below keybinding, pressing
;; F10 will take you to the original definition of whichever symbol
;; the cursor is on, no matter where it appears in the source
;; tree. Within a few presses of F10, you've escaped macro hell and
;; found where the code that actually defines the function you're
;; interested in is.


(defun find-tag-at-point ()
  "*Find tag whose name contains TAGNAME.
  Identical to `find-tag' but does not prompt for
  tag when called interactively;  instead, uses
  tag around or before point."
  (interactive)
  (find-tag (if current-prefix-arg
                (find-tag-tag "Find tag: "))
            (find-tag (find-tag-default))))

(defun search-forward-symbol-at-point ()
  "Search forward to next occurrence of thing at point"
  (interactive)
  (search-forward (symbol-name (symbol-at-point)) nil t))

(defun search-backward-symbol-at-point ()
  "Search forward to next occurrence of thing at point"
  (interactive)
  (search-backward (symbol-name (symbol-at-point)) nil t))

(defun quote-list-of-symbols ()
  "Place double quotes around the comma-separated,
parenthesis-delimited list of symbols at point"
  (interactive)
  (save-excursion
    (let ((beg (search-forward "("))
          (end (save-excursion (search-forward ")"))))
      (insert "\"")
      (while (replace-regexp " *, *" "\", \"" t beg end))
      (replace-regexp " *)" "\")" t (point) (1+ end)))))

(defun dan/find-defun (fun)
  (interactive "a")
  (describe-function fun)
  (other-window 1)
  (when (re-search-forward "`[^']+\.e")
    (push-button)))

(defun dan/wc-region ()
  (interactive)
  (shell-command-on-region (mark) (point) "wc"))

;; http://www.emacswiki.org/cgi-bin/wiki/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))


(defun byte-compile-dir (dir)
  (interactive)
  (let ((files (directory-files dir t ".*\.el" t)) file)
    (while (setq file (pop files))
      (byte-compile-file file))))


(defun budget-eval ()
  ;; to eval yanked text in python-shell -- doesn't work
  (interactive)
  (other-buffer)
  (yank)
  (newline))

;; (defun dan/xclip-kill ()
;;   "kill region and place on X clipboard"
;;   (interactive)
;;   (shell-command-on-region (mark) (point) "xclip")
;;   (delete-region (mark) (point))) ;; don't add to kill ring

;; (defun dan/xclip-yank ()
;;   "yank from X clipboard and insert at point"
;;   (interactive)
;;   (shell-command "xclip -o" t))

(defun dan/next-line-and-indent ()
  (interactive)
  (next-line)
  (indent-according-to-mode))

(defun dan/previous-line-and-indent ()
  (interactive)
  (previous-line)
  (indent-according-to-mode))

(defun dan/insert-square-brackets ()
  (interactive)
  (insert "[]")
  (backward-char))

(defun dan/insert-curly-brackets ()
  (interactive)
  (insert "{}")
  (backward-char))

(defun dan/enclose-sexp-in-parentheses ()
  (interactive)
  (insert "(")
  (forward-sexp)
  (insert ")"))

(defun dan/enclose-rest-of-line-in-parentheses ()
  (interactive)
  (insert "(")
  (end-of-line) ;; need to account for comment on same line
  (insert ")"))

(defun dan/insert-- ()
  (interactive)
  (insert "-"))

(defun dan/quote-word ()
  "Surround word at point with double quotes"
  (interactive)
  (re-search-backward "[ ,(\t]" nil t)
  (forward-char) (insert "\"")
  (re-search-forward "[ ,)\t]" nil t)
  (backward-char) (insert "\""))

(defun dan/compile-and-switch-to-iESS ()
  (interactive)
  (when (compile "make -k")
    (ess-switch-to-end-of-ESS)))

;;  (when (shell-command "make -k")

;; From Sacha Chua website
(defun byte-compile-if-newer-and-load (file)
  "Byte compile file.el if newer than file.elc"
  (if (file-newer-than-file-p (concat file ".el")
                              (concat file ".elc"))
      (byte-compile-file (concat file ".el")))
  (load file))

(defun dan/show-buffer-file-name ()
  (interactive)
  (let ((bn (buffer-name (current-buffer)))
        (bfn (buffer-file-name))
        (dd default-directory))
    (when bfn
      ;; file buffer
      (if (string= (file-name-nondirectory bfn) bn)
          ;; expected buffer name
          (if (string= (file-name-directory bfn) dd)
              ;; expected default-directory
              (message bfn)
            ;; unexpected default-directory
            (message "buffer-file-name: %s\tdefault-directory: %s" bn dd))
        ;; unexpected buffer name
        (if (string= (file-name-directory bfn) dd)
            ;; expected default-directory
            (message "buffer-file-name: %s\tbuffer-name: %s" bfn bn)
          ;; unexpected default-directory
          (message "buffer-file-name: %s\tbuffer-name: %s\tdefault-directory: %s" bfn bn dd)))
      (dan/save-value-to-kill-ring bfn))
    (message "buffer-file-name: %S\tbuffer-name: %s\tdefault-directory: %s" bfn bn dd)))

(defun dan/show-variable (&optional sym)
  (interactive "vVariable name: ")
  (message "%S" (eval sym)))

(defun dan/format-region-for-post (start end)
  (interactive "r")
  (narrow-to-region start end)
  (goto-char (point-min))
  (while (re-search-forward "^[ \t]+" nil t)
    (replace-match ""))
  (goto-char (point-min))
  (while (re-search-forward "\\([a-zA-Z]\\)\n\\([a-zA-Z]\\)" nil t)
    (replace-match "\1 \2" t t))
  (widen))

(setq dan--previous-window nil)

(defun dan--set-previous-window ()
  (setq dan--previous-window (selected-window)))

(defadvice other-window (before set-previous)
  "Set previous window before switching"
  (dan--set-previous-window))

(ad-activate 'other-window)

(defun dan/previous-window ()
  (interactive)
  (when dan--previous-window
    (let ((previous (selected-window)))
      (select-window dan--previous-window)
      (setq dan--previous-window previous))))

(defun dan/switch-windows ()
  "Switch the buffers between windows"
  (interactive)
  (let ((other-window-buffer (window-buffer (next-window))))
    (set-window-buffer (next-window) (current-buffer))
    (set-window-buffer (selected-window) other-window-buffer)))

(defun dan/find-file-emacs-config ()
  (interactive)
  (find-file
   (expand-file-name "~/config/emacs/emacs.org")))

;; (setq custom-file "~/src/config/emacs/emacs.el") ;; now code
;; generated by emacs' customisation buffers will go in this file rather
;; than ~/.emacs

;; Kevin Rodgers help-gnu-emacs
;; eldoc/timer can be used somehow to control how long messages appear for
;; (add-hook 'post-command-hook 'eldoc-schedule-timer nil t)
;; (add-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area t)
;; (setq eldoc-timer [nil 1000000 0 500000 t eldoc-print-current-symbol-info nil t]) ;;

(defun dan/current-column-line ()
  (let ((line (line-number-at-pos (point)))
        (col (current-column)))
    (message "line: %d\tcolumn: %d" line col)
    (list line col)))

(defun dan/eol-column-line (&optional arg)
  (interactive "P")
  (if arg (dan/current-column-line)
    (save-excursion
      (end-of-line)
      (dan/current-column-line))))

(defun dan/where-am-i (&optional arg)
  (interactive "P")
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'django-mode))
      (dan/python-where-am-i)
    (dan/eol-column-line arg)))

(defun dan/find-function-or-library (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'find-library 'find-function)))

(defun dan/find-function-at-point ()
  "Find directly the function at point.

Straight copy of `find-function-at-point` but using
`find-function` instead of `finf-function-other-window`."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))


(defun dan/require (feature)
  (unless (featurep feature)
    (if (locate-library (symbol-name feature))
        (require feature)
      (progn
        (message "Could not locate library: %s" (symbol-name feature))
        nil))))

;; based on starter-kit-defuns
(defun dan/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let* ((alist
          (mapcar
           (lambda (f)
             (cons (format "%s/%s"
                           (file-name-nondirectory
                           (substring (file-name-directory f) 0 -1))
                           (file-name-nondirectory f))
                   f))
                  recentf-list))
         (file (ido-completing-read "Find file: " (mapcar 'car alist) nil t)))
    (when file
      (find-file (cdr (assoc file alist))))))

(defun dan/find-file (&optional arg)
  (interactive "P")
  (call-interactively
   (cond
     ((not arg) 'dan/recentf-ido-find-file)
     ((equal arg '(4)) 'fuzzy-find-in-project)
     ((equal arg '(16)) 'ido-find-file))))

(defun dan/show-all-all-buffers ()
  (interactive)
  (save-window-excursion
    (mapc (lambda (b) (set-buffer b) (show-all))
          (buffer-list))))

(defun dan/switch-to-minibuffer ()
  (interactive)
  (switch-to-buffer (window-buffer (minibuffer-window))))

(defun dan/other-non-minibuffer-window ()
  (interactive)
  (while (progn (other-window 1)
                (window-minibuffer-p))))

(defun dan/scratch-buffer (&optional arg)
  "Scratch buffers for various major modes"
  (interactive "P")
  (let* ((modes
          `(("text-mode" . "txt")
            ("org-mode" . "org")
            ("python-mode" . "py")
            ("coffee-mode" . "coffee")
            ("emacs-lisp-mode" . "el")
            ("clojure-mode" . "clj")
            ("js-mode" . "js")
            ("html-mode" . "html")
            ("compilation-mode" . "compilation")
            ("sql-mode" . "sql")
            ("markdown-mode" . "md")))
         (buf-file-name (buffer-file-name (current-buffer)))
         (buf-mode
          (or (assoc (symbol-name major-mode) modes)
              (and buf-file-name
                   (cons (symbol-name major-mode)
                         (file-name-extension buf-file-name)))))
         (modes
          (if buf-mode
              (delete-dups (append (list buf-mode) modes))
            modes))
         (mode
          (ido-completing-read "Mode: " (mapcar #'car modes)))
         (mode-fun (intern mode))
         (contents
          (and (region-active-p)
               (prog1 (buffer-substring (region-beginning)
                                        (region-end))
                 (if arg (kill-region (region-beginning) (region-end)))))))
    (find-file (concat "/tmp/scratch." (cdr (assoc mode modes))))
    (unless (eq major-mode mode-fun) (funcall mode-fun))
    (when contents
      (delete-region (point-min) (point-max))
      (insert (org-remove-indentation contents)))))

(setq pop-up-windows t
      split-window-preferred-function 'split-window-sensibly
      split-width-threshold nil
      split-height-threshold nil)

(setq ns-use-native-fullscreen nil)

(if nil
    (defun dan/display-buffer-whole-frame (buffer &rest ignored)
      ;; (switch-to-buffer buffer)
      (delete-other-windows))

  (setq special-display-function 'dan/display-buffer-whole-frame)

  ;; (setq special-display-function (lambda (buffer &rest ignored) (switch-to-buffer buffer) (delete-other-windows))))
  (setq special-display-function (lambda (buffer &rest ignored) (delete-other-windows)))
  )

(defun dan/window-configuration (register &arg)
  (interactive "P")
  (cond
   ((not arg)
    (jump-to-register register)
    (message buffer-file-name))

   ((equal arg '(4))
    (window-configuration-to-register register)
    (message (char-to-string register)))

   ((equal arg '(16))
    (dan/list-window-configurations))))

(defun dan/list-window-configurations ()
  ;; copied from list-registers
  (interactive)
  (let ((list (copy-sequence register-alist))
        (temp-buffer-name "*window-configurations*")
        (temp-buffer-show-hook '(compilation-mode)))
    (setq list
          (dan/filter (lambda (elt) (and (window-configuration-p (second elt))
                                    (number-or-marker-p (first elt))))  ;; magit uses :magit-screen
                      list))
    (setq list (sort list (lambda (a b) (< (car a) (car b)))))
    (with-output-to-temp-buffer temp-buffer-name
      (dolist (elt list)
        (when (get-register (car elt))
          (let* ((label (single-key-description (first elt)))
                 (marker (third elt))
                 (buffer (marker-buffer marker))
                 (file-name (buffer-file-name buffer))
                 line column)
            (save-window-excursion
              (goto-char marker)
              (setq line (line-number-at-pos (point)))
              (setq column (current-column)))
              (princ
               (format "%s:%d:%d: %s" file-name line column label)))
          (terpri))))
    (select-window (get-buffer-window temp-buffer-name))))


(set-cursor-color "red")
(setq-default cursor-in-non-selected-windows nil)
(nconc default-frame-alist '((cursor-type . bar)))
(blink-cursor-mode -1)

(require 'org)
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
            (define-key outline-minor-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(add-hook 'outline-mode-hook
          (lambda ()
            (define-key outline-mode-map [(tab)] 'org-cycle)
            (define-key outline-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(defun dan/set-up-outline-minor-mode (outline-regexp)
  (set (make-local-variable 'outline-regexp) outline-regexp)
  (outline-minor-mode t)
  (org-overview)
  (org-content))

(add-hook 'python-mode-hook
          (lambda () (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)")))

(add-hook 'coffee-mode-hook
          (lambda () (dan/set-up-outline-minor-mode "[ \t]*\\(class .+\\)")))

;; (add-hook 'coffee-mode-hook
;;           (lambda () (dan/set-up-outline-minor-mode "[ \t]*\\(class .+\\|[^:]+:.+[=-]>\\)")))

(add-hook 'js-mode-hook
          (lambda () (dan/set-up-outline-minor-mode ".+\\(function .+\\)")))

(add-hook 'markdown-mode-hook
          (lambda () (dan/set-up-outline-minor-mode "##")))

(add-hook 'emacs-lisp-mode-hook
          (lambda () (dan/set-up-outline-minor-mode "\\((\\|;;;\\)")))
(add-hook 'ess-mode-hook
          (lambda ()
            (unless (eq noweb-code-mode 'R-mode)
              ;; (dan/set-up-outline-minor-mode "^\\(###\\|[a-zA-Z._[\"][a-zA-Z._0-9[\"]* *<- *function\\)")
              ;; (dan/set-up-outline-minor-mode "^[a-zA-Z._[\"][a-zA-Z._0-9[\"]* *<- *function")
              (dan/set-up-outline-minor-mode "[a-zA-Z._][a-zA-Z._0-9]* *<- *function"))))
;; (add-hook 'c-mode-hook
;;        (lambda () (dan/set-up-outline-minor-mode nil)))
;;                    "\\(void\\|int\\|double\\|char\\|struct\\|static\\|const\\)")))
(add-hook 'bibtex-mode-hook
          (lambda () (dan/set-up-outline-minor-mode "@")))

(setq tab-always-indent 'complete)
(setq dan/ignored-extensions
      '(".ps" ".bst" ".cls" ".pyc" ".elc"
        ".fdf" ".spl" ".aux" ".ppt" ".doc" ".xls" ".mp3"))

(setq completion-ignored-extensions
      (union completion-ignored-extensions
             dan/ignored-extensions :test 'equal))

(require 'ido)
(setq ido-ignore-files '("\\`#")
      ido-ignore-buffers '())

;; As regexps, these should really have terminal $
(mapc (lambda (extension)
        (add-to-list 'ido-ignore-buffers (regexp-quote extension))
        (add-to-list 'ido-ignore-files (regexp-quote extension)))
      dan/ignored-extensions)

(add-to-list 'ido-ignore-buffers "\\*") ;; if you want *scratch* or *R* just type it
(add-to-list 'ido-ignore-buffers " ") ;; hidden buffers

(add-to-list 'load-path "~/lib/emacs/winner-mode")
(require 'winner)
(let ((is-dired-buffer? (lambda (buff) (eq (with-current-buffer buff major-mode) 'dired-mode))))
  (add-to-list 'ido-ignore-buffers is-dired-buffer?)
  (add-to-list 'winner-boring-buffers is-dired-buffer?))

(setq completion-show-help nil)

;; (add-hook 'completion-list-mode-hook
;;           (lambda () 
;;             (unless (minibufferp (current-buffer)) (progn (goto-char (point-min))
;;                     (and (looking-at "Possible completions are:")
;;                          (kill-line 1))))))

;; Don't offer to save directories!
(setq compilation-save-buffers-predicate (lambda () nil))

(setq comint-input-ring-size 1024)

;; See ess-help post by M. Maechler on 23 Mar 2006
(eval-after-load
    "comint"
  '(progn
     (setq comint-scroll-to-bottom-on-output 'others) ; not current
     ;;=default: (setq comint-scroll-to-bottom-on-input nil)
     (setq comint-scroll-show-maximum-output t) ;;; this is the key
     (define-key comint-mode-map [C-up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [C-down]
       'comint-next-matching-input-from-input)
     (define-key comint-mode-map "\C-a" 'comint-bol)))

(show-paren-mode t)
(winner-mode t)
(global-font-lock-mode t)

;; (desktop-save-mode t)
;; (display-battery-mode t)
(global-auto-revert-mode t)
(setq auto-revert-interval 1)

(setq eval-expression-debug-on-error nil)
(setq find-function-C-source-directory "~/lib/emacs/emacs-23.1/src")

(setq ring-bell-function (lambda nil nil))
(setq case-fold-search nil)
(setq default-major-mode 'org-mode)
(setq diff-switches "-u")
(setq frame-title-format "emacs:%b") ;;      (concat  "%b - emacs@" (system-name)))
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(setq minibuffer-message-timeout 0.5)
(setq parens-require-spaces nil)
(setq require-final-newline 'visit-save)
(setq vc-follow-symlinks t)
(setq x-alt-keysym 'meta)
(setq backup-inhibited t)
(setq font-lock-verbose nil)
(setq truncate-lines t)
(setq tab-width 4)
(add-hook 'occur-mode-hook (lambda () (setq truncate-lines t)))
(add-hook 'compilation-mode-hook (lambda () (setq truncate-lines t)))
(add-to-list 'auto-mode-alist '("\\.compilation$" . compilation-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-mode))

;; DNW
;; Wrong type argument: window-live-p, nil
;; (add-hook 'compilation-mode-hook (lambda () (select-window (get-window-with-predicate (lambda (win) (eq (window-buffer win) (current-buffer)))))))
;; (remove-hook 'compilation-mode-hook (lambda () (select-window (other-window 1))))

;; apparently this doesn't work; must be run in a
;; save-window-configuration or something
;; (add-hook 'compilation-mode-hook 'delete-other-windows)





(setq indent-tabs-mode nil)

;; (visit-tags-table tags-file-name)
;; (setq font-lock-always-fontify-immediately t) where did I get that from?

(fset 'yes-or-no-p 'y-or-n-p) ;; http://www.xsteve.at/prg/emacs/.emacs.txt -- replace y-e-s by y
(put 'narrow-to-region 'disabled nil)

;; put back-up files in a single (invisible) directory in the original file's directory
;; (setq backup-directory-alist '(("." . ".emacs-backups")))
;; put back-up files in a single (invisible) directory in home directory -- doesn't work
;; (setq backup-directory-alist '(("~/.emacs-backups")))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (setq kill-buffer-query-functions '(lambda() t))

;; (transient-mark-mode t) ;; something turns it off

(setq source-directory (expand-file-name "~/lib/emacs/emacs/src"))

;; remove the -e flag to xargs, use 4 processes
(setq grep-find-command "find . -type f -print0 | xargs -P4 -0 grep -nH -e")
(setq grep-find-template "find . <X> -type f <F> -print0 | xargs -P4 -0 grep <C> -nH -e <R>")


;; This doesn't work with org-src-mode code buffers as their
;; buffer-file-name doesn't correspond to a file
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(defvar dan/delete-trailing-whitespace-major-modes
  '(python-mode ess-mode coffee-mode javascript-mode
                go-mode haskell-mode clojure-mode html-mode graphviz-dot-mode))

(defun dan/query-delete-trailing-whitespace ()
  "If there's trailing whitespace ask to delete it"
  (when (memq major-mode dan/delete-trailing-whitespace-major-modes)
    (unless buffer-read-only
      (save-excursion
        (save-window-excursion
          (save-restriction
            (goto-char (point-min))
            (and (re-search-forward "[ \t]$" nil t)
                 ;; (yes-or-no-p "Delete trailing whitespace?")
                 (delete-trailing-whitespace))))))))

(add-hook 'before-save-hook 'dan/query-delete-trailing-whitespace)

;; Make sure code is visible when jumping into it from compilation buffer
(add-hook 'next-error-hook #'show-all)
(add-hook 'find-function-after-hook #'show-all)


(defadvice goto-line (after reveal)
  "Ensure target location is not hidden"
  (show-all))

(ad-activate 'goto-line)

(defadvice compile-goto-error (after compile-goto-error-delete-other-windows)
  "Ensure target location is not hidden"
  (delete-other-windows))

(ad-activate 'compile-goto-error)

(setq message-send-mail-partially nil)

;; http://flash.metawaredesign.co.uk/2/.emacs
(let ((browser (if (eq dan/operating-system 'darwin)
                   "open"
                 (or (getenv "BROWSER") "google-chrome"))))
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program browser)
  (when (and browser (string-match browser "firefox"))
    (setq browse-url-firefox-new-window-is-tab t)))

(setq vc-handled-backends nil)
(setq vc-follow-symlinks t)

(add-to-list 'load-path "~/lib/emacs/winner-mode")
(require 'winner)

(add-to-list 'load-path "~/lib/emacs")

(add-to-list 'load-path "~/lib/emacs/ag.el")
(require 'ag)

(setq ag-arguments (append '("--ignore" "'*#'"
                             "--ignore" "'*.js'"
                             "--ignore" "'*.xml'"
                             "--ignore" "'*.log'"
                             "--ignore" "'*.sql'"
                             ;; "--ignore" "'*.txt'"
                             ;; "--ignore" "'*.md'"
                             "--ignore" "'*.wsdl'"
                             "--ignore" "'*.min.css'"
                             ;; "--ignore" "'*.html'"
                             ;; "--ignore" "'*.scss'"
                             ;; "--ignore" "'*.css'"
                             "--ignore" "'*.json'"
                             "--ignore" "'*.pdf'"
                             "--ignore" "'*.tsv'"
                             "--ignore" "'*.yaml'")
                           ag-arguments))

(add-hook
 'ag-mode-hook
 (lambda () (switch-to-buffer "*ag*") (delete-other-windows)))


(setq bookmark-sort-flag nil)

(setq ibuffer-show-empty-filter-groups nil)

(defalias 'list-buffers 'ibuffer)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("website"
          (name . "website/counsyl"))
         ("seq_pipeline"
          (name . "seq_pipeline"))
          ("VBPL"
          (or
           (name . "Papers/structure")
           (name . "^dan\.bib$")))
         ("PoBI"
          (name . "pobi"))
         ("WTCCC2"
          (name . "wtccc2"))
         ("MSG"
          (name . "simsec")
          (name . "Papers/msg"))
         ("shellfish"
          (name . "shellfish"))
         ("Org-babel"
          (name . "babel"))
         ("Org-mode"
          (or (name . "org-mode")
              (name . "^org\.org$")))
         ("Org-buffers"
          (name . "org-buffers"))
         ("Email"
          (or  ;; mail-related buffers
           (mode . message-mode)
           (mode . mail-mode)
           (mode . gnus-group-mode)
           (mode . gnus-summary-mode)
           (mode . gnus-article-mode)
           (name . "newsrc")))
         ("Elisp"
          (or
           (name . "config/emacs")
           (name . "^\\*scratch\\*$")
           (name . "^\\*eshell\\*$")))
         ("Emacs"
          (or
           (name . "^\\*scratch\\*$")
           (name . "^\\*Messages\\*$")))
         ("Org"
          (mode . org-mode))
         ("Python"
          (mode . python-mode))
         ("ERC"
          (mode . erc-mode))
         ("Etc"
          (name . ".")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq Buffer-menu-sort-column 4)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; setting mode-line-format to empty string triggers error in
;; ediff-strip-mode-line-format [Wrong type argument: listp, ""],
;; e.g. when issuing vc-resolve-conflicts
(defun dan/vc-resolve-conflicts ()
  (interactive)
  (let ((mode-line-format " "))
    (vc-resolve-conflicts)))

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-indentation 2)
            (local-set-key [(control return)] 'delete-other-windows)))

;; (setq emmet-move-cursor-between-quotes t) ;; default nil
;; (setq emmet-move-cursor-after-expanding nil) ;; default t

(recentf-mode t)
;; recentf-exclude
(setq recentf-max-saved-items nil)

(add-to-list 'load-path "~/lib/emacs/find-file-in-project")
(require 'find-file-in-project)
;; (setq ffip-find-options "\\( -path \\*/.git -o -path \\*/migrations -o -path \\*/build \\) -prune")


(add-to-list 'load-path "~/lib/emacs/fuzzy-find-in-project")
(require 'fuzzy-find-in-project)

(setq flyspell-issue-message-flag nil)

(require 'flymake)
(defun dan/flymake-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "/Users/dan/venvs/website/bin/codequality" (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.py\\'" dan/flymake-init))

(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" dan/flymake-init))

(defun dan/flymake ()
  (interactive)
  (call-interactively 'flymake-mode)
  (message (format "flymake %s" (if flymake-mode "on" "off"))))


(add-to-list 'load-path "~/lib/emacs/flymake-cursor")
(require 'flymake-cursor)

(setq flymake-log-level 1)


(setq dired-listing-switches "-lAX")
(setq dired-no-confirm
      '(byte-compile chgrp chmod chown compress copy hardlink load move print shell symlink
                     touch uncompress))
(setq dired-auto-revert-buffer t)

(require 'dired-x)
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode t)))

;; https://groups.google.com/group/gnu.emacs.help/browse_thread/thread/acb20ee78c00e4ec?pli=1
;; (setq dired-omit-files
;;       (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
;;               (seq bol "." (not (any "."))) ;; dot-files
;;               (seq "~" eol)                 ;; backup-files
;;               (seq bol "CVS" eol)           ;; CVS dirs
;;               )))

(defun dan/dired-delete-total-line ()
  (let ((bro buffer-read-only)
        (kill-whole-line t))
    (save-excursion
      (goto-char (point-min))
      (forward-line)
      (when (looking-at "^ *total used in directory")
        (if bro (setq buffer-read-only nil))
        (delete-region (line-beginning-position) (line-end-position))
        (setq buffer-read-only bro)))))

(add-hook 'dired-after-readin-hook 'dan/dired-delete-total-line)

(defun dan/dired-no-ask ()
  (interactive)
  (dired default-directory))

(setq dired-omit-extensions
      (append dan/ignored-extensions
              dired-latex-unclean-extensions
              dired-bibtex-unclean-extensions
              dired-texinfo-unclean-extensions))

(setq dired-omit-files (concat dired-omit-files "\\|^\\."))

(defun dan/dired-git-files ()
  (interactive)
  (dired (cons (format "%s [git]" default-directory)
               (dan/ls-git-files))))

(defun dan/ls-git-files ()
  (if (file-exists-p ".git")
      (split-string (shell-command-to-string "git ls-files"))
    (error "Not a git repo")))

(add-to-list 'load-path "~/lib/emacs/ruby-emacs")
(add-to-list 'load-path "~/lib/emacs/gnuplot-mode.0.6.0")
(add-to-list 'load-path "~/lib/emacs/matlab")

(add-hook 'emacs-lisp-mode-hook 'esk/pretty-lambdas)

;; Dan Feb 2006: See http://www.xemacs.org/Links/tutorials_1.html
(defun dan/c-c++-mode-hook ()
  "Dan's local settings for c-mode and c++-mode"
  ;; add font-lock to function calls (but also gets if() and while() etc)
  ;; (font-lock-add-keywords
  ;; ? ?nil `(("\\([[:alpha:]_][[:alnum:]_]*\\)(" ?1 font-lock-function-name-face)))
  (setq c-basic-offset 4)
  (setq line-number-mode t)
  (paredit-c-mode))

;; (add-hook 'c-mode-hook 'c++-mode) ;; I want C++ comments, but that seems a bit heavy-handed?
(add-hook 'c-mode-hook 'dan/c-c++-mode-hook)
(add-hook 'c++-mode-hook 'dan/c-c++-mode-hook)
(setq compilation-read-command nil)
(add-to-list 'auto-mode-alist '("\\.ino$" . c-mode))

(setq inferior-lisp-program "/usr/local/bin/clisp")
(add-to-list 'load-path "~/lib/emacs/slime/")

;; These are defined with flet in slime.el, but something seems to be
;; up with flet in emacs 24.
(defun remap (from to)
  (dolist (mapping (where-is-internal from slime-mode-map))
    (define-key slime-macroexpansion-minor-mode-map mapping to)))
(defun reader (slot) (intern (concat (symbol-name conc-name)
                                     (symbol-name slot))))

(require 'slime)
(slime-setup)

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode 1)))

(add-to-list 'load-path "~/lib/emacs/clojure-mode")
(require 'clojure-mode)
(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))

(defvar dan/hexcolour-keywords
  '(("#[abcdef[:digit:]]\\{6\\}"
     (0 (put-text-property
         (match-beginning 0)
         (match-end 0)
         'face (list :background
                     (match-string-no-properties 0)))))))

(defun dan/hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil dan/hexcolour-keywords))

(add-hook 'css-mode-hook 'dan/hexcolour-add-to-font-lock)

(load-file "~/lib/emacs/graphviz-dot-mode.el")
(setq graphviz-dot-indent-width 4)

(defun dan/set-after-save-command-compile-dot (&optional arg)
  (interactive)
  (let ((output-format (if arg "svg" "png"))
        (file-name ((buffer-file-name))))
    (dan/set-after-save-command
     (format
      "dot -T %s -o %s.%s %s"
      output-format
      (file-name-sans-extension file-name)
      output-format
      file-name))))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(add-to-list 'load-path "~/lib/emacs/coffee-mode")
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(setq coffee-tab-width 2)
(setq coffee-js-mode 'js-mode)
(defun dan/coffee-mode-hook-fun ()
  (set (make-local-variable 'tab-width) coffee-tab-width)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (paredit-c-mode))

(add-hook 'coffee-mode-hook 'dan/coffee-mode-hook-fun)
(defun dan/coffee-execute ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "coffee"))
(defun dan/coffee-insert-console-log ()
  (interactive)
  ;; (indent-for-tab-command) coffee indenting is bad
  (insert "console.log();")
  (backward-char 2))

(defun dan/coffee-insert-debugger ()
  (interactive)
  ;; (indent-for-tab-command) coffee indenting is bad
  (insert "debugger;"))

(add-hook 'after-save-hook
          (lambda () (when (eq major-mode 'coffee-mode) (coffee-compile-file))))


(add-to-list 'load-path "~/lib/emacs/go-mode.el")
(require 'go-mode)
;; (add-to-list 'load-path "~/lib/emacs/goflymake")
;; (require 'go-flymake)

(defun dan/go-hook-function ()
  (set (make-local-variable 'before-save-hook) '(gofmt))
  (setq tab-width 4)
  (paredit-c-mode))

(add-hook 'go-mode-hook 'dan/go-hook-function)


    (add-to-list 'load-path "~/lib/emacs/haskell-mode")
    (require 'haskell-mode)
    (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
    
    
    (defun dan/haskell-hook-function ()
      (turn-on-haskell-indentation)
      (paredit-c-mode)
      (local-set-key "'" 'self-insert-command))
    
    (add-hook 'haskell-mode-hook 'dan/haskell-hook-function)

;; (add-to-list 'load-path "~/lib/emacs/django-mode")
;; (require 'django-html-mode)
;; (require 'django-mode)
;; (add-to-list 'auto-mode-alist '("\\.html$" . django-html-mode))

(add-to-list 'load-path "~/lib/emacs/pony-mode/src")
(require 'pony-mode)

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(defun dan/js-mode-hook-fun ()
  (dan/coffee-mode-hook-fun)
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'dan/js-mode-hook-fun)


(defun dan/scratch-js-in-html ()
  (interactive)
  (find-file "/tmp/scratch.html")
  (delete-region (point-min) (point-max))
  (insert "<html>
<script type=\"text/javascript\">

</script>
</html>")
  (previous-line 2)
  (beginning-of-line))

(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
;; (autoload 'lua-mode "/usr/local/src/lua-mode/lua-mode" "Lua editing mode." t)
;; (add-hook 'lua-mode-hook 'turn-on-font-lock)

(require 'tex-mode)
(add-hook 'latex-mode-hook 'reftex-mode)
(add-hook 'latex-mode-hook (lambda () (setq truncate-lines t)))

(add-to-list 'load-path "~/lib/emacs/markdown-mode")
(require 'markdown-mode)
(setq auto-mode-alist (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq markdown-follow-wiki-link-on-enter nil)
(setq markdown-css-path (expand-file-name "~/lib/gollum/gollum.css"))
;; (add-hook 'markdown-mode-hook
;;           (lambda ()
;;             (longlines-mode 1)
;;             (setq longlines-wrap-follows-window-size t)))

(require 'texinfo)

(add-to-list 'auto-mode-alist '("\\.tweet$" . tweet-mode))

(dan/require 'plantuml-mode)

;; (defvar dan/python-exec-lines
;;   "import sys
;; import os
;; sys.path.append(os.path.expanduser('~'))
;; ")

;; %load_ext autoreload
;; %autoreload 2

(defun dan/python-eval-exec-lines ()
  "Hack until shellplus honors `exec_lines`"
  (interactive)
  (with-current-buffer "*Python*"
    (python-shell-send-string-no-output
     dan/python-exec-lines
     (get-buffer-process (current-buffer)))))

(defun dan/python (cmd split &optional restart)
  (let* ((buf-name "*Python*")
         (process-buffer (get-buffer buf-name)))
    (when (and restart process-buffer) (kill-buffer buf-name))
    (unless (and process-buffer (get-buffer-process process-buffer))
      (run-python (eval cmd))
      (python-shell-send-string dan/python-startup-string)
      ;; (dan/python-eval-exec-lines)
      ;; Start up clean
      ;; (sleep-for 5)
      ;; (delete-region (point-min) (point-max))
      (setq process-buffer (get-buffer buf-name)))
    (if split
        (set-window-buffer (split-window-below) process-buffer)
      (switch-to-buffer process-buffer))))

(defvar dan/ipython-command "ipython")
(defvar dan/python-startup-string
  (mapconcat 'identity
             '("from itertools import *"
               "from functools import *"
               "from collections import *"
               "from operator import *"
               "from django.db.models import Count"
               "import json")
             " ; "))

(defun dan/ipython (&optional arg)
  (interactive "P")
  (dan/python dan/ipython-command arg))

(defun dan/ipython-console (&optional arg)
  (interactive "P")
  (dan/python
   '(concat "~/lib/python/ipython/ipython.py console "
            (read-from-minibuffer "Arguments: " "--existing"))
   arg))

(defun dan/python-shell-clear ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-send-input))


;; -W ignore:Module:UserWarning %s

(add-hook 'python-mode-hook
          (lambda () (local-set-key "\C-c\C-z" #'dan/ipython)))
(global-set-key "\C-c\C-z" #'dan/ipython)

(defun dan/insert-ipdb-set-trace (&optional traceback)
  (interactive "P")
  ;; (indent-for-tab-command)
  (if traceback
      (insert "import traceback ; import ipdb ; print traceback.format_exc() ; ipdb.set_trace()")
  (insert "import ipdb ; ipdb.set_trace()")))

(defun dan/insert-import-numpy ()
  (interactive)
  (indent-for-tab-command)
  (insert "import numpy as np"))

(fset 'dict-literal-to-kwargs
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([3 114 up up return up up return] 0 "%d")) arg)))

;; (defun dan/python-dict-literal-to-kwargs ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (region-beginning))
;;     (while (re-search-forward "'\\([^']+\\)': *\\([^,]+\\)," (region-end) t)
;;       (replace-match
;;        (format "%s=%s," (match-string 1) (match-string 2))))))

;; (defun dan/python-kwargs-to-dict-literal ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (region-beginning))
;;     (while (re-search-forward "\\([^ =]+\\)" (region-end) t) ;; "\\([^ =]+\\)=\\([^ ]+\\),"
;;       (replace-match
;;        (format "'%s': %s," (match-string 1) (match-string 2))))))


(defun dan/python-dict-literal-to-kwargs ()
  (interactive)
  (replace-regexp
   "[\"']\\([^\"']+\\)[\"']: \\([^,]+\\),"
   "\\1=\\2,"
   nil (region-beginning) (region-end)))

(defun dan/python-kwargs-to-dict-literal ()
  (interactive)
  (replace-regexp
   " \\([^ =]+\\) *= *\\([^,\n]+\\),?\n"
   " '\\1': \\2,\n"
   nil (region-beginning) (region-end)))

(defun dan/python-prep-paste ()
  (interactive)
  (let ((frag (buffer-substring (region-beginning) (region-end))))
    (with-temp-buffer
      (insert frag)
      (org-do-remove-indentation 999)
      (goto-char (point-min))
      (replace-regexp "[\n \\]+" " ")
      (kill-ring-save (point-min) (point-max)))))

(defun dan/python-kill-ring-save (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'dan/python-prep-paste 'kill-ring-save)))

(defun dan/python-wrap-region (format-string)
  (let* ((beg (region-beginning))
         (end (region-end))
         (frag (dan/dedent (buffer-substring beg end)))
         (indent (python-indent-calculate-indentation))
         (insertion (dan/indent (format format-string frag) indent)))
    (delete-region beg end)
    (insert insertion)))

(defun dan/python-wrap-region-with-debug-info ()
  (interactive)
  (dan/python-wrap-region
   "from django.db import connection
import datetime
q0 = len(connection.queries)
t0 = datetime.datetime.now()

%s
q1 = len(connection.queries)
t1 = datetime.datetime.now()
print 'Queries: %%d' %% (q1 - q0)
print 'Time: ', (t1 - t0)
"))

(setq python-fill-docstring-style 'django)


(require 'python)
(setq auto-mode-alist (cons '("\\.pyw$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.pyx$" . python-mode) auto-mode-alist))

;; Recommended config from the code comments
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; My updates to config
(setq python-shell-prompt-regexp ">>> "
      python-shell-prompt-output-regexp "    ")
;; python-shell-interpreter-args "--colors=NoColor"

(setq python-shell-enable-syntax-highlighting nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; python comint history

(defvar dan/python-comint-history-file "~/.ipython/history")
(defvar dan/python-comint-history-max 1000)

(defun dan/load-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  (message "Loading python comint history...")
  (setq comint-input-ring
        (ring-convert-sequence-to-ring (dan/read-comint-history file))))

(defun dan/read-comint-history (file)
  (split-string (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)) "\n" t))

(defun dan/dump-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  ;; Most recent is first in comint-input-ring. Write file in
  ;; same order seeing as we are overwriting, not appending.
  (let ((history (org-uniquify (ring-elements comint-input-ring))))
    (setq history (subseq history 0 (min (length history)
                                         dan/python-comint-history-max)))
    (with-temp-buffer
      (insert (mapconcat #'identity history "\n") "\n")
      (write-file file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'kill-buffer-hook
          (lambda () (when (eq major-mode 'inferior-python-mode)
                  (dan/dump-comint-history dan/python-comint-history-file))))

(defun dan/inferior-python-mode-hook-function ()
  (esk/pretty-lambdas)
  (setq truncate-lines t)
  (dan/load-comint-history dan/python-comint-history-file))


(add-hook 'inferior-python-mode-hook
          'dan/inferior-python-mode-hook-function)

(defun dan/python-mode-hook-function ()
  (esk/pretty-lambdas)
  (setq forward-sexp-function nil) ; for use with paredit
  (add-to-list (make-local-variable 'comint-dynamic-complete-functions)
               'python-completion-complete-at-point))

(add-hook 'python-mode-hook
          'dan/python-mode-hook-function)

(add-hook 'python-mode-hook 'paredit-c-mode)


(defun dan/python-shell-send-chunk ()
  "Send the current chunk to inferior Python process."
  (interactive)
  (skip-chars-forward "\n")
  (python-shell-send-region
     (progn (backward-paragraph) (point))
     (progn (forward-paragraph) (point))))

(defun dan/python-current-defun-name ()
  (interactive)
  (save-excursion
    (let* ((get-name (lambda ()
                       (beginning-of-defun)
                       (looking-at python-nav-beginning-of-defun-regexp)
                       (match-string 1)))
           (names `(,(funcall get-name)))
           name)
      (when (looking-at "[ \t]")
        (while (looking-at "[ \t]")
          (setq name (funcall get-name)))
          (push name names))
      (message (mapconcat #'identity names ".")))))

(defun dan/python-where-am-i ()
  (interactive "P")
  (message
   (dan/save-value-to-kill-ring
    (if arg
        (format
         "website_test %s:%s"
         (replace-regexp-in-string
          ".__init__.py" ""
          (replace-regexp-in-string
           "/" "."
           (replace-regexp-in-string
            (concat "^" (counsyl--git-dir) "/") ""
            (replace-regexp-in-string
             "\.py$" "" (buffer-file-name)))))
         (dan/python-current-defun-name))
      (format "%s %s"
              (dan/python-current-defun-name)
              (dan/eol-column-line))))))

(defun dan/strip-quotes (string)
  (if (string-match "[\"']+\\(.+\\)[\"']+" string)
      (match-string 1 string)
    string))

(defun dan/import-at-point ()
  (interactive)
  (let ((end (point))
        (word (thing-at-point 'symbol)))
    (backward-word)
    (delete-region (point) end)
    (insert
     (format
      "from %s import %s"
      (dan/strip-quotes
       (python-shell-send-string-no-output
        (concat word ".__module__"))) word))))

;;; Change directory
(defun dan/python-cd (directory)
  "Change current directory in emacs and in the python process"
  (interactive "DChange directory: ")
  (let ((process (get-buffer-process (current-buffer)))
       (directory (expand-file-name directory)))
    (cd-absolute directory)
    (python-shell-send-string-no-output
     (format "import os; os.chdir('%s')" directory)
     process)))


(defun dan/rope-goto-definition-of-thing-read-from-minibuffer (string)
  (interactive "sGo to definition of: ")
  (save-excursion
    (goto-char (point-max))
    (unless (eq (char-before) ?\n) (insert ?\n))
    (insert string)
    (let ((buff (current-buffer)))
      (rope-goto-definition)
      (set-buffer buff)
      (kill-line 0))))

(defalias 'dan/rope-goto-definition-of-thing-at-point 'rope-goto-definition)

(defun dan/rope-goto-definition (&optional arg)
  (interactive "P")
  (push-mark)
  (call-interactively
   (if arg 'dan/rope-goto-definition-of-thing-read-from-minibuffer
     'dan/rope-goto-definition-of-thing-at-point)))

(defun python-import-bounds-of-python-import-at-point ()
   "Return the start and end points of python-import at current point."
   (let ((characters "A-Za-z_."))
     (save-excursion
       (re-search-backward (concat "[^" characters "]") nil t)
       (forward-char 1)
       (if (looking-at (concat "[" characters "]+"))
           (cons (point) (match-end 0))
         nil))))

(put 'python-import 'bounds-of-thing-at-point
     'python-import-bounds-of-python-import-at-point)

(add-to-list 'load-path "~/lib/emacs/scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook
          (lambda()
            (comint-send-input)
            (recenter-top-bottom 0)))
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

(eval-after-load "sql" '(sql-set-product 'postgres))

(add-hook 'sql-mode-hook (lambda () (sql-set-product 'postgres)))

(add-hook 'eshell-mode-hook 'dan/set-exec-path-from-shell)
(add-hook 'eshell-mode-hook 'paredit-mode)

(setq eshell-banner-message ""
      eshell-scroll-show-maximum-output nil)

(setq eshell-input-filter
      (lambda (str)
        (not
         (or
          ;; The default: don't store all whitespace
          (string-match "\\`\\s-*\\'" str)
          ;; Don't store consecutive identical input
          (string= str (nth 0 (ring-elements eshell-history-ring)))))))

(add-to-list 'load-path "~/lib/emacs/ess/lisp")
(when (dan/require 'ess-site)

  ;; (require 'ess-eldoc)

  (setq ess-ask-for-ess-directory t)
  (setq inferior-R-args "--no-save --no-restore-data --silent")
  (setq safe-local-variable-values '((noweb-default-code-mode . R-mode) (outline-minor-mode)))
  (autoload 'noweb-mode "noweb-mode" "Editing noweb files." t) ;; see noweb-mode.el in ESS;
  (setq auto-mode-alist (append (list (cons "\\.nw$" 'noweb-mode))
                                auto-mode-alist))

  ;; (defun dan/ess-and-iess-mode-hook ()
  ;;   (setq ess-function-template " <- function() {\n\n}\n")
  ;;   (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
  ;;        dan/ess-and-iess-keybindings))

  (defun dan/ess-mode-hook ()
    (ess-set-style 'C++))

  ;; (add-hook 'ess-mode-hook 'dan/ess-and-iess-mode-hook)
  ;; (add-hook 'inferior-ess-mode-hook 'dan/ess-and-iess-mode-hook)
  (add-hook 'ess-mode-hook 'dan/ess-mode-hook)

  (setq ess-eval-visibly-p nil)

  ;;                                 DEF GNU BSD K&R C++
  ;; ess-indent-level                  2   2   8   5   4
  ;; ess-continued-statement-offset    2   2   8   5   4
  ;; ess-brace-offset                  0   0  -8  -5  -4
  ;; ess-arg-function-offset           2   4   0   0   0
  ;; ess-expression-offset             4   2   8   5   4
  ;; ess-else-offset                   0   0   0   0   0
  ;; ess-close-brace-offset            0   0   0   0   0

  (defun dan/ess-execute-command-on-region (cmd)
    (interactive "sEnter function name: \n")
    (ess-execute
     (concat cmd "(" (buffer-substring (point) (mark)) ")"))))


(add-to-list 'load-path "~/lib/emacs/buffer-join")
;; (dan/require 'buffer-join)

(add-to-list 'load-path "~/lib/emacs/color-theme-6.6.0")
(dan/require 'color-theme)
;; (dan/require 'zenburn)
;; (dan/require 'color-theme-chocolate-rain)
(load-file "~/lib/emacs/color-theme-railscasts/color-theme-railscasts.el")
;; (color-theme-railscasts)

(package-initialize)
(add-to-list
 'package-archives
 '("marmalade" . "http://marmalade-repo.org/packages/"))



(defun dan/google ()
  (interactive)
  (shell-command
   (format "google '%s'"
           (if (region-active-p)
               (buffer-substring (region-beginning)
                                 (region-end))
             (read-from-minibuffer "Search string: ")))))

(add-to-list 'load-path "~/lib/emacs/google-maps")
(dan/require 'google-maps)

(add-to-list 'load-path "~/lib/emacs/google-weather-el")
(dan/require 'google-weather)
(dan/require 'org-google-weather)

(require 'hide-lines)

(add-to-list 'load-path "~/lib/emacs/mo-git-blame")
(require 'mo-git-blame)

(add-to-list 'load-path "~/lib/emacs/magit")
(add-to-list 'load-path "~/lib/emacs/git-modes")
(dan/require 'magit)
(require 'magit-blame)
(setq magit-save-some-buffers nil)
(setq magit-process-popup-time 1)
(setq magit-revert-item-confirm t)

(setq magit-status-sections-hook
  '(
    ;; magit-insert-status-local-line
    ;; magit-insert-status-remote-line
    ;; magit-insert-status-head-line
    ;; magit-insert-status-tags-line
    ;; magit-insert-status-merge-line
    ;; magit-insert-status-rebase-lines
    ;; magit-insert-empty-line
    ;; magit-insert-rebase-sequence
    ;; magit-insert-bisect-output
    ;; magit-insert-bisect-rest
    ;; magit-insert-bisect-log
    ;; magit-insert-stashes
    ;; magit-insert-untracked-files
    ;; magit-insert-pending-changes
    ;; magit-insert-pending-commits
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    ;; magit-insert-unpulled-commits
    ;; magit-insert-unpushed-commits
    ))

(add-hook 'magit-mode-hook (lambda () (setq truncate-lines t)))
;; (add-hook 'magit-diff-mode-hook 'dan/magit-hide-all-sections)

(defadvice magit-refresh-diff-buffer (after hide-sections-and-delete-other-windows)
  (dan/magit-hide-all-sections)
  (delete-other-windows))

(ad-activate 'magit-refresh-diff-buffer)

(set-face-attribute 'magit-diff-add nil :foreground "green") ;; darkgreen
(set-face-attribute 'magit-diff-del nil :foreground "red")

;; (set-face-attribute 'magit-item-highlight nil :background "default") ;; "#01294A" "#01395C"
(set-face-attribute 'magit-diff-hunk-header nil :background "#01294A") ;; "keyboardFocusIndicatorColor"
;; (set-face-attribute 'magit-item-highlight nil :background "secondarySelectedControlColor")



;; * http://whattheemacsd.com/
;; ** magit window management

;; (setq magit-status-buffer-name "magit-status: %t")

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)



(defun dan/magit-hide-all-sections ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((next))
      (while (setq next (magit-find-section-after (point)))
        (magit-goto-section next)
        (magit-hide-section)))))


(defun dan/magit-status (&optional arg)
  (interactive "P")
  (call-interactively 'magit-status)
  ;; (switch-to-buffer "*magit-status: website*")
  (unless arg (delete-other-windows)))


(defun dan/magit-kill-git-process ()
  (interactive)
  ;; (kill-process magit-process) ? doesn't do what I want.
  (kill-buffer magit-process-buffer-name))


;; magit uses defkey
(defalias 'defkey 'define-key*)

(defun dan/markdown-to-org (&optional arg)
  (interactive "P")
  (save-excursion
    (if arg (-dan/org-to-markdown)
      (-dan/markdown-to-org))))

(defun -dan/do-substitutions (substitutions)
  (mapc (lambda (substitution)
          (goto-char (point-min))
          (while (re-search-forward (car substitution) nil t)
            (replace-match (cdr substitution))))
        substitutions))

(defun -dan/markdown-to-org ()
  (-dan/do-substitutions
   '(("#" . "*")
     ("^```python" . "#+begin_src python")
     ("^```" . "#+end_src")))
  (org-mode))

(defun -dan/org-to-markdown ()
  (-dan/do-substitutions
   '(("\*" . "#")
     ("^#\\+begin_src python" . "```python")
     ("^#\\+end_src.*" . "```")))
  (markdown-mode))

(dan/require 'regex-tool)
(dan/require 'unbound)
(dan/require 'windresize)
(dan/require 'xclip)
(dan/require 'highlight-parentheses)
(highlight-parentheses-mode)
(dan/require 'boxquote)
;; (load "~/lib/emacs/nxhtml/autostart.el")
;; (load "R-anything-config")
(dan/require 'ssh)
;; (dan/require 'google-search)
;; (dan/require 'w3m)
;; (dan/require 'gnuplot)
;; (dan/require 'filladapt)

(add-to-list 'load-path "~/src/emacs/minimal")
(when (dan/require 'minimal)
  (minimal-mode t)
  (setq minimal-mode-line-background "sea green")
  (setq minimal-mode-line-inactive-background "dim grey"))

(add-to-list 'load-path "~/lib/emacs/paredit")
(require 'paredit)
(add-to-list 'load-path "~/src/emacs/paredit-c")
(require 'paredit-c)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (condition-case nil
                (paredit-mode)
              (error (message "Failed to activate paredit mode")))))

(defun dan/paredit-mode (&optional force)
  (interactive "P")
  (let (contents point)
    (when force
        ;; Get rid of anything that might upset paredit
        (setq contents (buffer-string)
              point (point))
        (delete-region (point-min) (point-max)))
    (if (eq major-mode 'emacs-lisp-mode)
        (paredit-mode)
      (paredit-c-mode))
    (when force (insert contents) (goto-char point))))


;; (add-hook 'ess-mode-hook 'paredit-c-mode)
;; if: Point is not in a function according to 'ess-function-pattern'.
;; -> redefine ess-beginning-of-defun to default to 'no-error
;; but now, on open smooth paren:
;; forward-list: Scan error: "Unbalanced parentheses", 21, 23 [2 times]

(fset 'ess-beginning-of-defun (lambda () (ess-beginning-of-defun 'no-error)))


(add-hook 'inferior-ess-mode-hook 'paredit-c-mode)
(add-hook 'comint-mode-hook 'paredit-c-mode)

;; http://stackoverflow.com/questions/2665292/how-can-i-get-paredit-mode-when-doing-eval-expression
(defun dan/configure-minibuffer-for-lisp ()
  (when (eq this-command 'eval-expression)
    (setq completion-at-point-functions '(lisp-completion-at-point t))
    (local-set-key [tab] 'complete-symbol)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'dan/configure-minibuffer-for-lisp)

(add-to-list 'load-path "~/lib/emacs/textmate.el")
(require 'textmate)

(require 'tramp) (condition-case nil (require 'tramp-sh) (error nil))
(setq tramp-remote-path (append tramp-remote-path (list "~/bin")))

(add-to-list 'load-path "~/lib/emacs/yasnippet")
(when (dan/require 'yasnippet)
  (setq yas/extra-mode-hooks nil)
  (setq yas/trigger-key "\C-cy")
  (define-key yas/keymap [tab] 'yas/next-field-group)
  (yas/initialize)
  (mapc (lambda (dir)
          (let ((dir (expand-file-name dir)))
            (if (file-exists-p dir) (yas/load-directory dir))))
        '("~/lib/emacs/yasnippet/snippets"
          "~/lib/emacs/Worg/org-contrib/babel/snippets"
          "~/lib/emacs/yasnippet-ess"
          "~/config/emacs/snippets"))

  (defun dan/yas-tab-setup ()
    ;; Initially by Eric for Org-mode hook
    (interactive)
;;    (make-variable-buffer-local 'yas/trigger-key)
;;    (setq yas/trigger-key [tab])
    (define-key yas/keymap [tab] 'yas/next-field-group)))

(yas/load-directory "~/lib/emacs/django-mode/snippets")

(defun dan/org-table-import ()
  (interactive)
  (let* ((table-file buffer-file-name)
         (org-buffer (concat (file-name-nondirectory table-file) ".org")))
    (when (get-buffer org-buffer)
      (kill-buffer org-buffer))
    (switch-to-buffer org-buffer)
      (org-mode)
      (org-table-import table-file nil)))

(defun dan/toggle-org-src-content-indentation ()
  (interactive)
  (message
   (format
    "Local content indentation set to %d"
    (org-set-local
     'org-edit-src-content-indentation
     (if (eq org-edit-src-content-indentation 0) 2 0)))))

(defun dan/org-toggle-display-inline-images-after-execution ()
  (interactive)
  (let ((hook 'org-babel-after-execute-hook)
        (fun 'org-display-inline-images))
    (message
     "Inline image display after execute %s"
     (if (memq fun (eval hook))
         (progn (remove-hook hook fun) "off")
       (add-hook hook fun) "on"))))

(defun dan/find-in-buffer ()
  (interactive)
  (let ((targets
         `(("<named src blocks>" . ,org-babel-src-name-regexp)
           ("<src block results>" . ,org-babel-result-regexp))))
    (occur
     (cdr
      (assoc
       (ido-completing-read "Find: " (mapcar #'car targets))
       targets)))
    (other-window 1)))

(defun dan/toggle-org-export-babel-evaluate ()
  (interactive)
  (message
   "org-export-babel-evaluate %s"
   (if (setq org-export-babel-evaluate (not org-export-babel-evaluate))
       "on" "off")))

(fset 'dan/org-search-in-buffer "\C-ca<s")

(defun dan/hide-subtree ()
  (interactive)
  (hide-subtree)
  (org-beginning-of-line))

(require 'cl)
(defun dan/org-babel-list-supported-languages ()
  (interactive)
  (sort
   (set-difference
    (mapcar
     (lambda (s) (intern (progn (string-match "^ob-\\(.+\\)\.el$" s)
                                (match-string 1 s))))
     (directory-files
      (save-window-excursion
        (file-name-directory
         (buffer-file-name (find-library "ob"))))
      nil "^ob-.+\.el$"))
    '(comint eval exp keys lob ref table tangle))
   (lambda (x y) (string< (downcase (symbol-name x))
                          (downcase (symbol-name y))))))

(defun dan/org-show-all ()
  (interactive)
  (let ((org-hide-block-startup nil))
    (org-mode)
    (show-all)))

(setq dan/org-mode-src-dir "~/lib/emacs/org")

(defun dan/org-search-src ()
  "Search for REGEXP in Org-mode source code."
  (interactive)
  (lgrep
   (if (region-active-p)
       (buffer-substring (region-beginning) (region-end))
     (org-completing-read "Regexp: "))
   "*.el" (concat dan/org-mode-src-dir "/lisp")))

(defun ml/org-grep (search &optional context)
  "Search for word in org files.

Prefix argument determines number of lines."
  (interactive "sSearch for: \nP")
  (let ((grep-find-ignored-files '("#*" ".#*"))
        (grep-template (concat "grep <X> -i -nH "
                               (when context
                                 (concat "-C" (number-to-string context)))
                               " -e <R> <F>")))
    (lgrep search "*org*" "/home/dan/org/")))

(global-set-key (kbd "<f7>") 'ml/org-grep)

(defun dan/org-edit-special ()
  (interactive)
  (if (save-excursion
        (re-search-forward
         (concat "\\("
                 org-babel-src-block-regexp
                 "\\|"
                 "^[ \t]*|" ;; table
                 "\\)") nil t))
      (org-edit-special)
    (message "No target found")))

(defun org-insert-link-maybe ()
  "Insert a file link depending on the context"
  (interactive)
  (let ((case-fold-search t))
    (if (save-excursion
          (when (re-search-backward "[[:space:]]" nil t)
            (forward-char 1)
            (looking-at "\\[?\\[?file:?\\(?:[ \t\n]\\|\\'\\)")))
        (progn (replace-match "") (org-insert-link '(4)) t)
      nil)))

;; (add-hook 'org-tab-first-hook 'org-insert-link-maybe)

(defun org-magit-store-link ()
  "Store a link to a directory to open with magit."
  (when (eq major-mode 'magit-mode)
    (let* ((dir default-directory)
           (link (org-make-link "magit:" dir))
           (desc (abbreviate-file-name dir)))
      (org-store-link-props :type "magit" :link link :description desc)
      link)))

(defun org-magit-open (dir)
  "Follow a magit link to DIR."
  (magit-status dir))

(org-add-link-type "magit" 'org-magit-open nil)
(add-hook 'org-store-link-functions 'org-magit-store-link)

(defun dan/org-read-subtrees ()
  "Return subtrees as a list of strings"
  (let ((subtrees))
    (while (or (looking-at "^*") (outline-next-heading))
      (outline-mark-subtree)
      (setq subtrees (cons (buffer-substring (point) (mark)) subtrees))
      (goto-char (mark)))
    (nreverse subtrees)))

(defun dan/org-reverse-subtrees ()
  "Reverse the order of all subtrees.

Should start by setting restriction?
"
  (interactive)
  (beginning-of-line)
  (let ((subtrees (dan/org-read-subtrees)))
    (beginning-of-buffer)
    (delete-region (point) (mark))
    (insert (mapconcat 'identity (nreverse subtrees) "\n"))))

(defun dan/htmlize-buffer-with-org-images ()
  "Convert buffer to html, including embedded images."
  (interactive)
  (save-excursion
    (switch-to-buffer (htmlize-buffer (current-buffer)))
    (goto-char (point-min))
    (while (re-search-forward "<span class=\"org-link\">file:\\(.+\\)</span>" nil t)
      (replace-match (concat "<img src='\\1'/>")))))

(defun eric/update-org-buffer ()
        "Update an Org-mode buffer to the new data, code block and call line syntax."
        (interactive)
        (save-excursion
          (flet ((to-re (lst) (concat "^[ \t]*#\\+" (regexp-opt lst t)
                                      "\\(\\[\\([[:alnum:]]+\\)\\]\\)?\\:[ \t]*"))
                 (update (re new)
                         (goto-char (point-min))
                         (while (re-search-forward re nil t)
                           (replace-match new nil nil nil 1))))
            (let ((old-re (to-re '("RESULTS" "DATA" "SRCNAME" "SOURCE")))
                  (lob-re (to-re '("LOB")))
                  (case-fold-search t))
              (update old-re "name")
              (update lob-re "call")))))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path (expand-file-name "~/lib/emacs/org/contrib/lisp"))
(require 'org-latex)

(dan/require 'org-inlinetask)
(dan/require 'org-special-blocks)

;; (org-indent-mode t)
;; (add-hook 'org-mode-hook 'dan/yas-tab-setup)

(setq org-hide-block-startup nil)

;; (setq org-startup-folded nil)
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


;; was near saveplace code; not sure whether helpful
(add-hook 'org-mode-hook
          (lambda ()
            (when (outline-invisible-p)
              (save-excursion
                (outline-previous-visible-heading 1)
                (org-show-subtree)))))

(defun dan/org-tables-to-markdown-tables ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (replace-regexp "-\\+-" "-|-"))))

(defun dan/org-tables-to-markdown-tables-set-hook ()
  (interactive)
  (add-hook 'before-save-hook 'dan/org-tables-to-markdown-tables))


(setq org-hide-leading-stars t)
(setq org-hidden-keywords '(title date author))

;; (setq org-odd-levels-only t)
(setq org-empty-line-terminates-plain-lists t)
(setq org-return-follows-link t)
(setq org-special-ctrl-a/e t)
(setq org-cycle-emulate-tab t)

(setq org-use-speed-commands t)

;;* remember
(org-remember-insinuate)
(setq org-default-notes-file "~/org/etc.org")
;; (setq org-remember-default-headline "top")
(setq org-remember-templates
      '(
        ("work" ?w "* TODO %?\nSCHEDULED: %^T  %i" "~/org/work.org" 'top)
        ("task" ?t "* TODO %?\nSCHEDULED: %^T\n  %i" "~/org/tasks.org" 'top)
        ("event" ?e "* %?\n%^T\n %i" "~/org/events.org" 'top)
        ("computing" ?c "* TODO %?\n  %i" "~/org/computing.org" 'top)
        ("org" ?o "* TODO %?\n  %i" "~/org/org.org")
        ("notes" ?n "* %?\n  %i" "~/org/notes.org" 'top)
        ("dbm" ?d "* TODO %?\n  %i" "~/org/dbm.org" 'top)
        ("music" ?m "* %?\n %i" "~/org/music.org" 'top)
        ("people" ?p "* TODO %?\nSCHEDULED: %^T\n  %i" "~/org/people.org" 'top)
        ("info" ?i "* %?\n %i" "~/zzz/info.org" 'top)
        ))

(defun dan/org-schedule-task-with-link (remember-target-char &optional arg)
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

(setq org-footnote-auto-label 'plain)

(setq org-todo-keywords
      '((sequence
         "TODO(t!@/!@)"
         "FIXME(f!@/!@)"
         "QUESTION(q!@/!@)"
         "STARTED(s!@/!@)"
         "WAIT(w!@/!@)"
         "|"
         "DONE(d!@/!@)"
         "POSTPONED(p!@/!@)"
         "ANSWERED(a!@/!@)"
         "CANCELLED(c!@/!@)"
         "DUPLICATE(u!@/!@)")))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(setq org-agenda-files
      (case dan/operating-system
        ('darwin '("~/Work" "~/org"))
        ('linux '("~/org"))))
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-ndays 30)
(setq org-agenda-compact-blocks t)
(setq org-deadline-warning-days 7)
(set-face-foreground 'org-agenda-date-weekend "blue")

(setq org-agenda-custom-commands
      `(("Q" "Search for items in state" todo "QUESTION")
        ("W" "Search for work items in state" todo "TODO"
         ((org-agenda-files '("~/org/work.org"
                              "~/org/wtccc2.org"
                              "~/org/pobi.org"
                              "~/org/shellfish.org"))))
        ("T" "Search for tasks items in state" todo "TODO"
         ((org-agenda-files '("~/org/tasks.org"))))
        ("C" "Search for computing items in state" todo "TODO"
         ((org-agenda-files '("~/org/computing.org"))))))

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
  ;;;         dayname day monthname year weekstring)

    (format "%s %2d %s"
            (substring dayname 0 3) day (substring monthname 0 3))))

(setq org-agenda-format-date 'org-agenda-format-date-aligned-dan)

(require 'org-html)
(setq org-export-htmlize-output-type (if t 'inline-css 'css))
(unless (member "svg" org-export-html-inline-image-extensions)
  (setq org-export-html-inline-image-extensions
        (cons "svg" org-export-html-inline-image-extensions)))

(setq org-export-with-LaTeX-fragments t)
(setq org-export-with-sub-superscripts nil)
(setq org-export-copy-to-kill-ring nil)
(setq org-export-allow-BIND t)

;; from Eric
(setq org-export-html-style
      "<style type=\"text/css\">
pre {
    border: 1pt solid #AEBDCC;
    background-color: #232323;
    color: #E6E1DC;
    padding: 5pt;
    font-family: courier, monospace;
    font-size: 90%;
    overflow:auto;
}
</style>")

;; (setq org-export-html-style
;; "<style type=\"text/css\">
;; pre {
;;     border: 1pt solid #AEBDCC;
;;     padding: 5pt;
;;     font-family: courier, monospace;
;;     font-size: 90%;
;;     overflow:auto;
;; }
;; </style>")

(setq org-latex-to-pdf-process '("rubber -fd --into %o %f"))

(defun dan/org-src-mode-hook ()
  (save-excursion
    (outline-minor-mode -1)))
;; why this python indent stuff?
;; (if (eq major-mode 'python-mode)
;;     (setq python-indent 4)))

(add-hook 'org-src-mode-hook 'dan/org-src-mode-hook)

(add-hook 'org-src-mode-hook
          ;; Note this is a poor choice of key for an org-src buffer
          ;; displaying an Org block
          (lambda () (define-key org-src-mode-map "\C-c\C-v"
                       'org-src-do-key-sequence-at-code-block)))
(defun dan/org-fill-paragraph-no-op-in-code-block ()
  (interactive)
  (if (org-babel-where-is-src-block-head)
      (message "In code block: doing nothing")
    (call-interactively 'fill-paragraph)))

(defun dan/org-babel-edit-src-code (&optional arg)
  (interactive "P")
  (if arg
      (org-babel-do-in-edit-buffer
       (org-edit-src-exit))
    (call-interactively 'org-edit-src-code)))

(setq org-src-window-setup 'current-window) ;; 'current-window 'other-window 'other-frame 'reorganize-frame

(setq org-src-ask-before-returning-to-edit-buffer nil)

(setq org-edit-src-content-indentation 0)

(setq org-edit-src-persistent-message nil)

(add-hook 'org-babel-post-tangle-hook
          (lambda () (when (eq major-mode 'coffee-mode) (coffee-compile-file))))

(defcustom org-src-native-commands '()
  "List of org-src native commands.

These are commands that, when issued in a src block, should be
performed 'natively', i.e. their effect should be as if they were
issued in a language major-mode buffer."
  :group 'org-babel
  :type '(set :greedy t
              (const tab)
              (const underscore)
              (const comment-dwim)
              (const indent-region)
              (const fill-paragraph)))

(setq org-src-native-commands
      '(tab underscore comment-dwim indent-region fill-paragraph))

(setq org-src-tab-acts-natively t)

(defun org-src-native/underscore ()
  (interactive)
  (or (and (memq 'underscore org-src-native-commands)
           (org-babel-do-key-sequence-in-edit-buffer "_"))
      (org-self-insert-command 1)))

(defun org-src-native/comment-dwim (&optional arg)
  (interactive "P")
  (or (and (memq 'comment-dwim org-src-native-commands)
           (org-babel-do-key-sequence-in-edit-buffer "\M-;"))
      (comment-dwim arg)))

(defun org-src-native/indent-region ()
  (interactive)
  (or (and (memq 'indent-region org-src-native-commands)
           (org-babel-do-key-sequence-in-edit-buffer "\C-\M-\\"))
      (indent-region)))

(defun org-src-native/fill-paragraph ()
  (interactive)
  (or (and (memq 'fill-paragraph org-src-native-commands)
           (org-babel-do-key-sequence-in-edit-buffer "\M-q"))
      (call-interactively 'fill-paragraph)))

(org-babel-lob-ingest "~/org-mode/contrib/babel/library-of-babel.org")

(setq org-babel-detect-errors-in-sessions t)

(defun dan/org-hide-block-and-switch-to-code-buffer ()
  (interactive "P")
  (let* ((beg (org-babel-where-is-src-block-head))
         (org-src-window-setup 'reorganize-frame))
    (when beg
      (goto-char beg)
      (org-hide-block-toggle 'hide)
      (org-edit-src-code))))

(setq org-babel-load-languages
      (mapcar (lambda (lang) (cons lang t))
              (dan/org-babel-list-supported-languages)))

(org-babel-do-load-languages
 'org-babel-load-languages org-babel-load-languages)

(setq swank-clojure-binary "/usr/bin/clojure")
(setq org-babel-js-cmd "node")
(setenv "NODE_DISABLE_COLORS" "1")

(add-to-list 'org-src-lang-modes '("C" . c))
(add-to-list 'org-src-lang-modes '("dot" . something-else))

(setq org-src-lang-modes
      (append '(("dot" . graphviz-dot))
              (delq (assoc "dot" org-src-lang-modes)
                    org-src-lang-modes)))

(setq org-src-fontify-natively t)
(setq org-babel-min-lines-for-block-output 10)
(setq org-export-babel-evaluate t)
(setq org-babel-noweb-error-langs
      (mapcar #'symbol-name (dan/org-babel-list-supported-languages)))

(defun dan/org-edit-src-code:current-window ()
  (interactive)
  (let ((org-src-window-setup 'current-window))
    (org-edit-src-code)))

(defun dan/org-edit-src-code:reorganize-frame ()
  (interactive)
  (let ((org-src-window-setup 'reorganize-frame))
    (org-edit-src-code)))

(defun org-babel-edit-special-maybe ()
  "Switch to edit buffer for block at point"
  (interactive)
  (let ((case-fold-search t)
        (org-src-window-setup 'current-window))
    (if (save-excursion
          (beginning-of-line 1)
          (looking-at org-babel-src-block-regexp))
        (progn (org-edit-special)
               t) ;; to signal that we took action
      nil))) ;; to signal that we did not

;; (add-hook 'org-tab-first-hook 'org-babel-edit-special-maybe)

(fset 'reset-tests
      (lambda (&optional arg) "Keyboard
   macro." (interactive "p") (kmacro-exec-ring-item (quote ([11
                                                             11 up 25 up 134217848 114 101 45 115 101 97 114 tab 98 97 99
                                                             tab return 84 66 76 78 65 77 69 return down 1 67108896
                                                             134217848 114 101 45 115 101 114 backspace 97 114 99 104 tab
                                                             102 111 tab return 84 66 76 70 77 return 1 134217848 114 101
                                                             45 114 101 112 tab backspace backspace backspace backspace 112
                                                             108 tab 105 110 tab M-backspace M-backspace M-backspace tab
                                                             114 101 tab 103 tab return 92 91 92 93 return return 134217848
                                                             up up return 84 66 76 70 77 return 1 11 11 down 25 up]
                                                            0 "%d")) arg)))

(setq org-startup-with-inline-images t)

(add-to-list 'load-path "~/lib/emacs/org-mode/contrib/lisp")

(dan/require 'htmlize)

(when (dan/require 'org-mime)

  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-htmlize)))

  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key "\C-c\M-o" 'org-mime-org-buffer-htmlize))))

(add-to-list 'load-path "~/lib/emacs/org-fstree")

(add-to-list 'load-path "~/lib/emacs/org-contacts")
(require 'org-contacts)
(setq org-contacts-files '("/Users/davison/config/email/contacts.org"))
(setq org-contacts-completion-ignore-case t)

(add-to-list 'load-path "~/lib/emacs/org-buffers")
(when (dan/require 'org-buffers)
  (defun dan/set-org-buffers-visibility ()
    (if (org-buffers-state-eq :atom 'heading)
        (org-overview)))
  ;; (add-hook 'org-buffers-mode-hook 'dan/set-org-buffers-visibility)
  ;; (setq special-display-buffer-names `(,org-buffers-buffer-name))
  )

(setq org-buffers-include-recent-files nil)

(add-to-list 'load-path "~/src/emacs/org-csv")
(require 'org-csv)

(add-to-list 'load-path "~/lib/emacs/session/lisp")
(require 'session)
(session-initialize)

(add-to-list 'load-path "~/lib/emacs/smex")
(when (dan/require 'smex)
  (smex-initialize)
  ;;(global-set-key (kbd "M-x") 'smex)
  ;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
  ;; (global-set-key (kbd "C-c C-x M-x") 'execute-extended-command)
  )

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-min-dir-content 1)

(add-to-list 'load-path "~/lib/emacs/python-django.el")
(require 'python-django)

(define-key global-map [(f2)] 'identity)

(dan/register-key-bindings
 '(global-map .
              (("\C-b" . backward-sexp)
               ("\C-f" . forward-sexp)
               ("\C-r" . isearch-backward-regexp)
               ("\C-s" . isearch-forward-regexp)
               ("\C-\M-r" . search-backward-symbol-at-point)
               ("\C-\M-s" . search-forward-symbol-at-point)
               ("\C-\\" . dan/indent-region)
               ([67108910] . blink-matching-open) ;; C-.
               ("\C-x\C-b" . org-buffers-list)
               ("\C-x\C-f" . dan/find-file)
               ("\C-x\M-f" . find-file-at-point)
               ("\C-xd" . dan/dired-no-ask)
               ("\C-xp" . dan/previous-window)
               ("\C-ca" . org-agenda)
               ("\C-cb" . org-iswitchb)
               ("\C-cd" . dan/delete-matching-lines)
               ("\C-cy" . yas/expand)
               ("\C-n" . dan/next-line-and-indent)
               ("\C-p" . dan/previous-line-and-indent)
               ("\C-ca" . org-agenda)
               ("\C-c\C-a" . show-all)
               ("\C-ce" . show-all)
               ("\C-cf" . dan/find)
               ("\C-c\M-f" . dan/find)
               ("\C-cg" . dan/magit-status)
               ("\C-ck" . bury-buffer)
               ("\C-ci" . dan/eol-column-line)
               ("\C-c\M-l" . bookmark-bmenu-list)
               ("\C-cl" . bookmark-bmenu-list)
               ("\C-cm" . bookmark-set)
               ("\C-cn" . dan/show-buffer-file-name)
               ("\C-co" . dan/scratch-buffer)
               ("\C-cp" . dan/paredit-mode)
               ("\C-cr" . replace-regexp)
               ("\C-cv" . dan/show-variable)
               ("\C-cw" . delete-window)
               ("\C-c)" . blink-matching-open)
               ("\C-c," . flymake-display-err-menu-for-current-line)
               ("\C-c\C-z" . dan/ipython)
               ("\C-z" . dan/dontfuckingdothat)
               ("\C-c\M-r" . rgrep)
               ("\C-\M-g" . lgrep)
               ("\C-x1" . (lambda () (interactive) (message "M-return")))
               ("\C-x\C-c" . kill-emacs)  ;; If it's not saved already that's your fault
               ("\C-x\C-q" . dan/toggle-read-only)
               ([(control next)] . end-of-buffer)
               ([(control prior)] . beginning-of-buffer)
               ([(control tab)] . ido-switch-buffer)
               ([(meta tab)] . pcomplete)
               ("\M-(" . dan/enclose-rest-of-line-in-parentheses)
               ("\M-n" . forward-paragraph)
               ("\M-p" . backward-paragraph)
               ("\M-o" . dan/occur)
               ("\M-i" . counsyl/highlight)
               ([delete] . winner-undo)
               ([(hyper left)] . winner-undo)
               ([(hyper right)] . winner-redo)
               ([(control left)] . winner-undo)
               ([(control right)] . winner-redo)
               ([(super o)] . dan/open-github-path-from-clipboard)
               ([(super left)] . winner-undo)
               ([(super right)] . winner-redo)
               ([(super return)] . fullscreen-mode-fullscreen-toggle)
               ([(meta up)] . org-metaup)
               ([(super down)] . scroll-up-line)
               ([(super up)] . scroll-down-line)
               ([f1] . dan/list-window-configurations)
               ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
               ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
               ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
               ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
               ([f6] . dan/find-file-emacs-config)
               ([f11] . dan/find)
               ([f12] . magit-status)
               ([(control return)] . delete-other-windows)
               ([(control escape)] . delete-window)
               ([(meta escape)] . delete-other-windows)
               ([escape] . dan/other-non-minibuffer-window))))

(global-set-key (kbd "s-,") 'dan/show-buffer-file-name)
(global-set-key (kbd "s-i") 'dan/where-am-i)


(add-hook 'after-change-major-mode-hook
          (lambda ()
            (local-set-key [delete] 'winner-undo)
            (local-set-key [(super left)] 'winner-undo)
            (local-set-key [(super right)] 'winner-redo)))


(require 'cc-mode)
(dan/register-key-bindings
 '("c" . nil))

(dan/register-key-bindings
 '(ctl-x-4-map .
               (("t" . toggle-window-split)
                ("s" . dan/switch-windows))))

(dan/register-key-bindings
 '("coffee" .
   (("\C-c\C-l" . coffee-compile-file)
    ("\C-c\C-r" . coffee-compile-region)
    ("\C-c\C-c" . dan/coffee-execute)
    ("\C-cd" . dan/coffee-insert-debugger)
    ("\C-cl" . dan/coffee-insert-console-log)
    ([(return)] . newline)
    ("\C-j" . newline-and-indent)
    ([(meta shift right)] . python-indent-shift-right)
    ([(meta shift left)] . python-indent-shift-left))))

(require 'dired)
(dan/register-key-bindings
 '("dired" .
   (([(left)] . (lambda () (interactive) (if buffer-read-only (dired-up-directory) (call-interactively 'left-char))))
    ([(right)] . (lambda () (interactive) (if buffer-read-only (dired-find-file) (call-interactively 'right-char)))))))

(dan/register-key-bindings
 '("emacs-lisp" .
   (("\C-cd" . edebug-defun)
    ("\C-c\C-l" . dan/eval-buffer-confirm)
    ("\M-so" . occur)
    ("\M-." . dan/find-function-at-point)
    ([tab] . dan/indent-or-complete)
    ([(control left)] . winner-undo)
    ([(control right)] . winner-redo))))

(dan/register-key-bindings
 '("ess" .
   (("\C-c?" . ess-display-help-on-object)
    ("\C-ca" . ess-r-args-show)

    ;; reverse the default bindings of these two
    ("\C-c\C-c" . ess-eval-function-and-go)
    ("\C-c\M-f" . ess-eval-function-or-paragraph-and-step)

    ("\C-cd" . dan/ess-list-R-function-definitions)
    ("\C-ck" . dan/ess-kill-line-and-indent)
    ("\C-cx" . dan/ess-recover-R-process)
    ([(control return)] . delete-other-windows)
    ([(shift tab)] . ess-complete-object-name))))

(dan/register-key-bindings
 `("inferior-ess" .
   ,(cdr (assoc "ess" dan/key-bindings))))

(dan/register-key-bindings
 '("haskell" .
   (("\C-c\C-c" . compile))))

(require 'js)
(dan/register-key-bindings
 '("js" .
   (("\C-cd" . dan/coffee-insert-debugger)
    ([(meta shift right)] . python-indent-shift-right)
    ([(meta shift left)] . python-indent-shift-left))))

(dan/register-key-bindings
 '("gnus-summary" .
   (("\C-d" . dan/gnus-summary-delete-article)
    ("\C-ct" . dan/gnus-summary-tick-thread))))

(require 'gnus-topic) ;; in order that mode-map exists

(dan/register-key-bindings
 '("gnus-topic" .
   (([tab] . gnus-topic-select-group)
    ([(meta right)] . gnus-topic-indent)
    ([(meta left)] . gnus-topic-unindent))))



(dan/register-key-bindings
 '("gnus-article" .
   (("\C-n" . dan/gnus-article-goto-next-article))))

(require 'hideshow)
(dan/register-key-bindings
 '(hs-minor-mode-map .
    (([C-tab] . hs-toggle-hiding)
     ([(shift tab)] . hs-hide-all))))

(dan/register-key-bindings
 '("latex" .
   (([C-tab] . TeX-complete-symbol))))

(dan/register-key-bindings
 '("markdown" .
   (([(meta left)] . left-word)
    ([(meta right)] . right-word))))

(dan/register-key-bindings
 '("mml" .
   (("\M-q" . ded/mml-fill-paragraph))))

(dan/register-key-bindings
 '("org" .
   (("\C-ch" . dan/hide-subtree)
    ("\C-c\M-i" . org-table-insert-column)
    ("\C-c\M-d" . org-table-delete-column)
    ("\C-cp" . org-insert-property-drawer)
    ("\C-ct" . org-hide-block-toggle)
    ("\C-cu" . (lambda () (interactive) (org-back-to-heading)))
    ("\C-c\C-z" . dan/ipython)
    ("_" . org-src-native/underscore)
    ("\M-q" . org-src-native/fill-paragraph)
    ("\M-;" . org-src-native/comment-dwim)
    ("\C-\\" . org-src-native/indent-region)
    ([(control \')] . dan/org-hide-block-and-switch-to-code-buffer)
    ([(control return)] . delete-other-windows)
    ([(meta left)] . org-metaleft)
    ([(meta right)] . org-metaright))))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "\M-\C-n") 'outline-next-visible-heading)
            (local-set-key (kbd "\M-\C-p") 'outline-previous-visible-heading)
            (local-set-key (kbd "\M-\C-u") 'outline-up-heading)))

(dan/register-key-bindings
 '("org-src" .
   (([(control \')] . org-edit-src-exit)
    ([delete] . org-edit-src-exit))))

(dan/register-key-bindings
 '("paredit" .
   (([(control left)] . winner-undo)
    ([(control right)] . winner-redo)
    ([(super down)] . scroll-up-line)
    ([(super up)] . scroll-down-line))))

(dan/register-key-bindings
 '("inferior-python" .
   (("\C-c\C-z" . dan/ipython)
    ("\C-c\M-n" . dan/insert-import-numpy)
    ("\C-cd" . dan/python-cd)
    ("\C-l" . dan/python-shell-clear)
    ("\M-." . dan/rope-goto-definition))))

(dan/register-key-bindings
 '("python" .
   (("\C-c\C-z" . dan/ipython)
    ("\C-cd" . dan/insert-ipdb-set-trace)
    ("\C-ci" . dan/python-where-am-i)
    ("\C-c\M-n" . dan/insert-import-numpy)
    ("\C-c\C-c" . python-shell-send-buffer)
    ("\C-xrm" . (lambda () (interactive) (bookmark-set (dan/python-current-defun-name))))
    ("\C-cm" . (lambda () (interactive) (bookmark-set (dan/python-current-defun-name))))
    ("\M-." . dan/rope-goto-definition)
    ("\M-w" . dan/python-kill-ring-save)
    ("\C-c," . flymake-goto-next-error)
    ("\C-ca" . (lambda () (interactive) (insert "# TODO-MOPA: accounts will have multiple customerprofiles")))
    ("\C-cs" . counsyl/sort-paragraph-at-point)
    ("\C-c\M-p" . dan/python-prep-paste)
    ([(meta shift right)] . python-indent-shift-right)
    ([(meta shift left)] . python-indent-shift-left)
    ([(meta up)] . org-metaup)
    ([tab] . dan/indent-or-complete))))

(dan/register-key-bindings
 '("texinfo" .
   (("\C-c\C-s" . dan/texinfo-show-structure))))

(defun dan/texinfo-show-structure (&optional nodes-too)
  (interactive)
  (texinfo-show-structure)
  (let ((buffer-read-only nil))
    (goto-char (point-min))
    (if (re-search-forward "^ +[0-9]+:" nil t)
        (delete-region (point-min) (point-at-bol)))))

(add-hook 'find-file-hook 'dan/sanitise-faces)
(add-hook 'find-file-hook 'dan/set-show-paren-style)
(setq mouse-highlight nil)

(add-to-list 'load-path "~/lib/emacs/emacs-color-theme-solarized")
(require 'color-theme-solarized)

(tool-bar-mode -1)
(menu-bar-mode -1)
(if (featurep 'scroll-bar)
    (scroll-bar-mode -1))
(setq inhibit-startup-message t)

(color-theme-initialize)
(case dan/operating-system
  ('linux
   (set-face-attribute 'default nil :height 110 :family "DejaVu Sans Mono"))
  ('darwin
   (set-face-attribute 'default nil :height 125)))
;; :weight 'bold :family 'default :foundry 'default :font 'unspecified
(dan/sanitise-faces)
(dan/set-show-paren-style)
(nconc default-frame-alist '((cursor-type . bar)))
;; (set-face-attribute 'org-block-begin-line nil :box nil :overline nil)
;; (set-face-attribute 'org-block-end-line nil :box nil :underline nil)

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))

(setq dan/frame-transparent-alpha '(100 100)) ;; (95 50) high -> opaque

(when nil
  (set-frame-parameter (selected-frame) 'alpha dan/frame-transparent-alpha)
  (add-to-list 'default-frame-alist (cons 'alpha dan/frame-transparent-alpha)))

(eval-when-compile (require 'cl))
(defun dan/toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (find 'alpha (frame-parameters nil) :key #'car))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha dan/frame-transparent-alpha)))

;;(when (string-match "^23\.*" emacs-version)
;; temp hack to make w3m work with emacs23
;; (require 'w3m-e21)
;; (provide 'w3m-e23))

;; (org-fireforg-registry-initialize t)

(if (eq dan/operating-system 'linux)
    (nnmairix-update-database))
(dan--set-key-bindings)
(dan/keys-enforce-global-bindings)
(if nil
    (org-agenda-list)
  (delete-other-windows))
(require 'server)
(unless (server-running-p) (server-start))
(ido-mode +1)
(setq ido-separator " ")
(eshell)

(setq scroll-preserve-screen-position :always
      scroll-conservatively           most-positive-fixnum
      scroll-step                     0)

(setq x-select-enable-primary t)


(progn
  (fset 'unhighlight nil)
  (fset 'highlight nil)
  (fset 'next-face nil)
  (fset 'ap nil)
  (fset 'e-merge nil))

(add-hook 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (if (looking-back "/")
           (insert "~/")
         (call-interactively 'self-insert-command))))))

