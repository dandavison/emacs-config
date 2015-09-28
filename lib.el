;;; Etc
(defun dan/switch-to-buffer (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'switch-to-buffer 'projectile-switch-to-buffer)))


(defun dan/find-file (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'find-file 'projectile-find-file)))


(defun dan/set-exec-path-from-shell (&optional pathvar)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (shell-command-to-string
	   (format "/bin/bash -c '. ~/.bashrc && echo -n $%s'" pathvar))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))

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

;;; After-save hook

(defun dan/set-after-save-command (cmd)
  (interactive "sCommand: ")
  (set (make-local-variable 'dan/after-save-command) cmd))

(defvar dan/after-save-command nil
  "This string will be executed as a shell command after saving
  the buffer.")

(defun dan/do-after-save-command ()
  (when dan/after-save-command
    (message dan/after-save-command)
    (save-window-excursion
      (async-shell-command dan/after-save-command))))

(add-hook 'after-save-hook 'dan/do-after-save-command)

;;; Trailing whitespace

(defvar dan/delete-trailing-whitespace-major-modes
  '(clojure-mode
    coffee-mode
    ess-mode
    go-mode
    graphviz-dot-mode
    haskell-mode
    html-mode
    javascript-mode
    puppet-mode
    python-mode
    makefile-bsdmake-mode))

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


;;; Appearance
;; From emacs-starter-kit
(defun dan/pretty-lambdas ()
  (interactive)
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


;;; Dired

(defun dan/dired-no-ask ()
  (interactive)
  (let ((current-filename (buffer-file-name (current-buffer))))
    (dired default-directory)
    (when current-filename (dired-goto-file current-filename))))


;;; Completion
(defun dan/indent-or-complete ()
  (interactive)
  (if (and (looking-at "[ \n)]")
           (looking-back "[^ \n]"))
      (complete-symbol nil)
    (indent-for-tab-command)))


;;; Search

(require 'ag)
(defvar dan/ag-arguments)

(defun dan/search (&optional arg)
  "Search for word at point in current git repository.

  With prefix arg prompt for search term."
  (interactive "P")
  (let* ((string (if arg (read-from-minibuffer "Regexp: ")
                   (or (thing-at-point 'symbol)
                       (error "No word at point"))))
         (directory (dan/git-get-git-dir))
         (backend 'git-grep))
    (switch-to-buffer "*search*")
    (delete-other-windows)
    (setq default-directory directory)
    (let ((buffer-read-only nil))
      (delete-region (point-min) (point-max))
      (save-excursion
        (insert (shell-command-to-string
                 (dan/make-search-command string backend)))))
    (compilation-mode)))


(defun dan/make-search-command (string backend)
  (mapconcat
   #'shell-quote-argument
   (case backend
     ('ag
      (append '("ag")
              dan/extra-ag-arguments
              ag-arguments
              (list string ".")))
     ('git-grep
      (list "git" "grep" "-n" string))
     (t (error "Invalid backend")))
   " "))


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

;;; Highlight
(require 'ring)
(setq dan/highlighted nil)
(setq dan/highlight-face 'trailing-whitespace)

(defun dan/highlight (&optional arg)
  "Toggle highlighting of word at point.

   With prefix arg, read the word to be highlighted from the
   minibuffer."
  (interactive "P")
  (cl-flet ((highlight (word)
                       (add-to-list 'dan/highlighted word)
                       (highlight-regexp word dan/highlight-face))
            (unhighlight (word)
                         (setq dan/highlighted nil)
                         (unhighlight-regexp word)))
    (let ((word (if arg (read-from-minibuffer "Highlight: ")
		  (thing-at-point 'symbol))))
      (when word
	(if (member word dan/highlighted)
	    (unhighlight word)
	  (highlight word))))))


;;; Outline

(require 'org)
(defun dan/set-up-outline-minor-mode (outline-regexp)
  (set (make-local-variable 'outline-regexp) outline-regexp)
  (outline-minor-mode t)
  (org-overview)
  (org-content))


;;; Windows

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


;;; Key bindings

;; Example:
;;
;; Add code like the following to your .emacs file to specify key
;; bindings in different major modes:
;;
;; (dan/register-key-bindings
;;  '(global-map .
;;               (("\C-c\M-g" . dan/grep))))
;; (require 'python)
;; (dan/register-key-bindings
;;  '("python" .
;;    (("\C-ci" . dan/python-import)
;;     ([f5] . dan/open-in-github))))
;;
;; More examples of key sequences:
;; [delete], [(super o)], [(control next)], [f4], [(meta up)]
;; super is OS X cmd key; binding some modifier keys may not be
;; possible if you are using emacs in a terminal.
;; See http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html

(defvar dan/key-bindings nil
  "List of all key bindings.
This is an alist of alists. The key of the top level alist
references a key map. If the key is a string, the string
\"-mode-map\" is appended to it when finding the mode-map. If it
is a symbol, it is used as is.")


(defun dan/register-key-bindings (bindings-alist)
  "Add bindings in dan/key-bindings"
  (setq
   dan/key-bindings
   (cons
    bindings-alist
    (dan/assoc-delete-all (car bindings-alist) dan/key-bindings)))
  (dan/set-key-bindings)
  nil)


(defun dan/set-key-bindings (&optional mode-map in-mode-map)
  "Set custom key bindings
Optional argument MODE-MAP sets bindings in that mode only
Optional argument IN-MODE-MAP sets MODE-MAP bindings in IN-MODE-MAP
"
  (interactive)
  (mapc (lambda (pair)
          (let* ((map (or in-mode-map (car pair)))
                 (bindings (cdr pair)))
            (if (stringp map) (setq map (intern (concat map "-mode-map"))))
            (if (symbolp map) (setq map (eval map)))
            (mapc (lambda (binding)
                    (define-key map (car binding) (cdr binding)))
                  bindings)))
        (or (and mode-map `(,(assoc mode-map dan/key-bindings)))
            dan/key-bindings)))


;;; Git
(defun dan/open-in-github (&optional clipboard-only)
  "Open current file location in github.
With C-u prefix argument copy URL to clipboard only."
  (interactive "P")
  (let ((git-dir (dan/git-get-git-dir)))
    (if git-dir
	(let* ((repo-url (dan/git-get-repo-url))
	       (commit (dan/git-get-commit))
	       (path (replace-regexp-in-string
		      (concat "^" git-dir) "" (buffer-file-name)))
	       (line (line-number-at-pos (point)))
	       (url (format
		     "%s/blob/%s%s#L%d"
		     repo-url commit path line)))
	  (if clipboard-only
	      (progn (kill-new url) (message url))
	    (browse-url url)))
      (message "Not in a git repo"))))



;;; Markdown
(defun dan/grip (&optional server)
  (interactive "P")
  (if server
      (async-shell-command
       (format "grip --wide %s" (buffer-file-name (current-buffer))))
    (let ((temp-file (make-temp-file "grip-")))
      (shell-command
       (format
        "grip --export --wide %s %s && open -a /Applications/Google\\ Chrome.app %s"
	  (buffer-file-name (current-buffer))
	  temp-file
	  temp-file)))))


;;; Python

(defun dan/insert-ipdb-set-trace (&optional traceback)
  (interactive "P")
  ;; (indent-for-tab-command)
  (let ((debugger "ipdb")) ;; pudb
    (insert
     (format
      (if traceback
          "import traceback ; import %s ; print traceback.format_exc() ; %s.set_trace()"
        "import %s ; %s.set_trace()")
      debugger debugger))))


(defun dan/python-set-virtualenv (path)
  (interactive "Dvirtualenv location: ")
  (setq python-shell-virtualenv-path path))

;;; Utilities

(defun dan/assoc-delete-all (key alist)
  "Like `assq-delete-all' but using `equal' for comparison"
  (delq nil
        (mapcar (lambda (el) (unless (equal (car el) key) el))
                alist)))

(defun dan/git-get-git-dir ()
  "Root dir of current repo"
  (file-name-as-directory
   (org-babel-chomp
    (shell-command-to-string "git rev-parse --show-toplevel"))))

(defun dan/git-get-commit ()
  "Current commit"
  (org-babel-chomp
   (shell-command-to-string "git rev-parse HEAD")))


(defun dan/git-get-repo-url ()
  (let ((remote-uri
         (org-babel-chomp
          (shell-command-to-string "git config --get remote.origin.url"))))
    (if (string-match "\\([^@]+\\)@\\([^:]+\\):\\([^.]+\\).git" remote-uri)
        (let ((protocol (match-string 1 remote-uri))
              (hostname (match-string 2 remote-uri))
              (repo (match-string 3 remote-uri)))
          (format "https://%s/%s" hostname repo))
      (error "Failed to parse URI: %s" remote-uri))))

(defun dan/save-value-to-kill-ring (&optional sexp)
  (interactive "XExpression to evaluate and save to kill-ring: ")
  (with-temp-buffer
    (let ((string (format "%s" sexp)))
      (insert string)
      (kill-ring-save (point-min) (point-max))
      string)))
