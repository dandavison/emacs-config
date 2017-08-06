;;; Etc

(defun dan/transpose-line-up ()
  (interactive)
  (save-excursion (transpose-lines 1))
  (previous-line))

(defun dan/transpose-line-down ()
  (interactive)
  (next-line)
  (save-excursion (transpose-lines 1)))

(defun dan/shell-command-on-region-and-replace ()
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'shell-command-on-region)))

(defun dan/switch-to-buffer (&optional arg)
  (interactive "P")
  (call-interactively
   (if (and (not arg) (projectile-project-p))
       'projectile-switch-to-buffer
     'switch-to-buffer)))

(defun dan/find-file (&optional arg)
  (interactive "P")
  (call-interactively
   (if (and (not arg) (projectile-project-p))
       'projectile-find-file
     'ido-find-file)))

;; Is this useful?
(defun dan/find-file-maybe-in-project (file)
  (find-file
   (if (projectile-project-p)
       (expand-file-name file (projectile-project-root))
     file)))

(defun dan/set-exec-path-from-shell (&optional pathvar)
  (interactive)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (shell-command-to-string
           (format "/bin/bash -c '. ~/.bashrc && echo -n $%s'" pathvar))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))

(defun dan/where-am-i (&optional arg)
  (interactive "P")
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'django-mode))
      (dan/python-where-am-i arg)
    (dan/show-buffer-file-name)))

(defun dan/show-buffer-file-name ()
  (interactive)
  (let ((bn (buffer-name (current-buffer)))
        (bfn (buffer-file-name)))
    (dan/save-value-to-kill-ring bfn)
    (message "%s        %s" bfn bn)))

(defun dan/show-buffer-file-name-complex ()
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

(defun dan/delete-matching-lines (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'delete-non-matching-lines 'delete-matching-lines)))

;;; Indentation

(defun dan/indent-shift-left (&rest args)
  (interactive)
  (let ((python-indent-offset 2))
    (call-interactively 'python-indent-shift-left)))

(defun dan/indent-shift-right (&rest args)
  (interactive)
  (let ((python-indent-offset 2))
    (call-interactively 'python-indent-shift-right)))


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

(defvar dan/no-delete-trailing-whitespace-major-modes nil)

(defun dan/query-delete-trailing-whitespace ()
  "If there's trailing whitespace ask to delete it"
  (unless (memq major-mode dan/no-delete-trailing-whitespace-major-modes)
    (unless buffer-read-only
      (save-excursion
        (save-window-excursion
          (save-restriction
            (goto-char (point-min))
            (and (re-search-forward "[ \t]$" nil t)
                 ;; (yes-or-no-p "Delete trailing whitespace?")
                 (delete-trailing-whitespace))))))))


;;; Appearance

(defun dan/set-appearance ()
  (interactive)
  (scroll-bar-mode -1)
  (set-cursor-color "red")
  (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (setq-default cursor-in-non-selected-windows nil)
  (setq cursor-type 'bar)
  (blink-cursor-mode -1)

  (set-face-background 'fringe (face-background 'default))
  (dan/set-show-paren-style))

(defun dan/set-show-paren-style ()
  (show-paren-mode t)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  (set-face-attribute 'show-paren-match nil :weight 'extra-bold)
  (set-face-background 'show-paren-match (face-background 'default))
  (set-face-attribute 'show-paren-match nil :foreground "red"))

(defun dan/theme-load ()
  (interactive)
  (call-interactively 'load-theme)
  (dan/set-appearance))

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

;;; Comment
(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position))
              end (save-excursion
                    (goto-char (region-end))
                    (line-end-position)))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

;;; Bookmarks

(defun dan/bookmark-set ()
  (interactive)
  (if (eq major-mode 'python-mode)
      (dan/python-bookmark-set)
    (call-interactively 'bookmark-set)))


;;; Search

;; deprecated for helm-swoop?
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
(setq dan/highlight-face 'hi-green)

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
    (let ((word (cond
                 (arg (read-from-minibuffer "Highlight: "))
                 ((region-active-p) (buffer-substring (region-beginning) (region-end)))
                 (t (thing-at-point 'symbol)))))
      (when word
	(if (member word dan/highlighted)
	    (unhighlight word)
	  (highlight word))
    (when (region-active-p) (deactivate-mark))))))


;;; Scratch buffers
(defvar dan/scratch-buffer-dir)

(defun dan/scratch-buffer (&optional arg)
  "Scratch buffers for various major modes"
  (interactive "P")
  (let* ((modes
          `(("clojure-mode" . "clj")
            ("coffee-mode" . "coffee")
            ("compilation-mode" . "compilation")
            ("emacs-lisp-mode" . "el")
            ("html-mode" . "html")
            ("js-mode" . "js")
            ("markdown-mode" . "md")
            ("org-mode" . "org")
            ("python-mode" . "py")
            ("sql-mode" . "sql")
            ("text-mode" . "txt")))
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
    (find-file (concat (file-name-as-directory dan/scratch-buffer-dir) "scratch." (cdr (assoc mode modes))))
    (unless (eq major-mode mode-fun) (funcall mode-fun))
    (when contents
      (delete-region (point-min) (point-max))
      (insert (org-remove-indentation contents)))))



;;; Outline

(require 'org)
(defun dan/set-up-outline-minor-mode (outline-regexp)
  (set (make-local-variable 'outline-regexp) outline-regexp)
  (outline-minor-mode t)
  (org-overview)
  (org-content))


;;; Paired characters

;; hack: use one of the many pairing modes
(defun dan/paired-character (open close)
  (if (region-active-p)
      (progn
        (save-excursion
          (goto-char (region-beginning))
          (insert open))
        (save-excursion
          (goto-char (region-end))
          (insert close)))
    (insert open close)
    (backward-char)))

(defun dan/paired-dollar ()
  (interactive)
  (dan/paired-character ?$ ?$))

(defun dan/paired-paren ()
  (interactive)
  (dan/paired-character ?\( ?\)))

(defun dan/paired-brace ()
  (interactive)
  (dan/paired-character ?{ ?}))


(defun dan/setup-paired-characters ()
  (interactive)
  (local-set-key "{" 'dan/paired-brace)
  (local-set-key "(" 'dan/paired-paren)
  (local-set-key "$" 'dan/paired-dollar))

;;; Windows

(defun dan/toggle-windows-frames (&optional arg)
  (interactive "P")
  (if arg
      (progn
        (local-set-key [(shift left)] 'other-frame)
        (local-set-key [(shift right)] 'other-frame)
        (message "frames"))
    (progn
      (windmove-default-keybindings)
      (message "windows"))))

(defun dan/maximize (&optional arg)
  (interactive "P")
  (if (not arg) (delete-other-windows)
    (toggle-frame-fullscreen)))

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
  (interactive)
  (let ((list (copy-sequence register-alist))
        (buffer-name "*window-configurations*"))
    (setq list
          (-filter (lambda (elt) (and (window-configuration-p (second elt))
                                 (number-or-marker-p (first elt))))
                   list))
    (setq list (sort list (lambda (a b) (< (car a) (car b)))))
    (with-current-buffer  (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (dolist (elt list)
          (when (get-register (car elt))
            (let* ((label (single-key-description (first elt)))
                   (marker (third elt))
                   (buffer (marker-buffer marker))
                   (file-name (buffer-file-name buffer))
                   project line column)
              (when file-name
                (with-current-buffer (marker-buffer marker)
                  (goto-char marker)
                  (setq project
                        (file-name-nondirectory
                         (directory-file-name (projectile-project-root))))
                  (setq line (line-number-at-pos (point)))
                  (setq column (current-column)))
                (let ((link (format "%s:%d:%d:" file-name line column)))
                  (insert (format "%s     %-40s %s"
                                  link (format "<%s>" project) label))
                  (put-text-property
                   (point-at-bol) (+ 1 (point-at-bol) (length link))
                   'display
                   (format "%-20s" (file-name-nondirectory file-name))))
                (insert ?\n))))))
      (goto-char (point-min))
      (compilation-mode))
    (set-window-buffer nil buffer-name)))


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
               (commit "master") ; (dan/git-get-commit)
               (path (replace-regexp-in-string
                      (concat "^" (file-truename git-dir)) ""
                      (file-truename (buffer-file-name))))
               (line (line-number-at-pos (point)))
               (url (format
                     "%s/blob/%s/%s#L%d"
                     repo-url commit path line)))
          (if clipboard-only
              (progn (kill-new url) (message url))
            (browse-url url)))
      (message "Not in a git repo"))))


;;; LaTeX
(defun dan/latex-fill-paragraph ()
  (interactive)
  (text-mode)
  (call-interactively 'fill-paragraph)
  (latex-mode))

(defun dan/latex-frac-region ()
  (let ((s (buffer-substring (region-beginning) (region-end))))
    (when (string-match "\\([^/]+\\)/\\([^/]+\\)" s)
      (delete-region (region-beginning) (region-end))
      (insert (format "\\frac{%s}{%s}" (match-string 1 s) (match-string 2 s))))))

(defun dan/latex-frac-back ()
  (when (looking-back "\\([^/]+\\)/\\([^/]+\\)")
    (let ((numerator (match-string 1))
          (denominator (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (format "\\frac{%s}{%s}"  numerator denominator)))))

(defun dan/latex-frac ()
  (interactive)
  (or (dan/latex-frac-region) (dan/latex-frac-back)))

(defun dan/focus ()
  (interactive)
  (when (not (region-active-p))
    (error "There is no active region."))
  (save-mark-and-excursion
   (condition-case nil (comment-region
     (region-beginning)
     (save-excursion
       (goto-char (region-beginning))
       (search-backward "\\begin{document}")
       (forward-line +1)
       (point))) (error nil))
   (condition-case nil (comment-region
     (region-end)
     (save-excursion
       (goto-char (region-end))
       (search-forward "\\end{document}")
       (forward-line -1)
       (point))) (error nil)))
  (narrow-to-region (region-beginning) (region-end)))

(defun dan/unfocus ()
  (interactive)
  (let ((beg (point-min))
        (end (point-max)))
    (widen)
    (condition-case nil
        (uncomment-region
         beg
         (save-excursion
           (goto-char beg)
           (search-backward "\\begin{description}")
           (forward-line +1)
           (point)))
     (error nil))
   (condition-case nil
       (uncomment-region
        end
        (save-excursion
          (goto-char end)
          (search-forward "\\end{description}")
          (forward-line -1)
          (point)))
     (error nil))))

(defun dan/latex-watch (&optional arg)
  (interactive "P")
  (dan/set-after-save-command
   (if (or arg t)
       ;;
       (format "/Users/dan/src/3p/rubber/build/scripts-2.7/rubber -d --shell-escape %s" buffer-file-name))))

;;; Eplot
(defun dan/eplot-region ()
  (interactive)
  (let ((tmp-file "/tmp/eplot.png"))
    (shell-command-on-region (region-beginning) (region-end)
                             (format "eplot -q -P -o %s" tmp-file))
    (insert-image (create-image tmp-file))))

;;; Magit

(defun dan/magit-hide-all-sections ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (while t
        (magit-section-forward)
        (magit-section-hide (magit-current-section))))))

(defun dan/magit-profile ()
  "https://github.com/magit/magit/issues/2228"
  ;; (elp-restore-all)
  (elp-instrument-package "magit")
  ;; (elp-reset-all)
  (magit-show-commit "8f8903f")
  ;; (magit-status "/path/to/repo")
  (elp-results))

;;; Markdown
(defun dan/grip (&optional server)
  (interactive "P")
  (dan/grip- (buffer-file-name (current-buffer)) server))

(defun dan/grip-code-region (&optional server)
  (interactive "P")
  (let ((code (buffer-substring (region-beginning) (region-end)))
        (lang (replace-regexp-in-string "-mode$" "" (symbol-name major-mode)))
        (temp-file-name (make-temp-file "dan/grip-code-region")))
    (with-temp-file temp-file-name
      (insert (format "```%s\n%s\n```" lang code)))
    (dan/grip- temp-file-name t)))

(defun dan/grip- (file-name server)
  (if server
      (async-shell-command
       (format "grip --browser --wide %s --api-url=https://github.counsyl.com/api/v3 --user=dan $(free-port)" file-name))
    (let ((temp-file (make-temp-file "grip-")))
      (shell-command
       (format
        "grip --browser --export --wide %s %s"
        file-name
        temp-file
        temp-file)))))

(defun dan/markdown-image-to-html (beg end)
  "Convert all markdown image links in region to HTML img tags."
  (interactive "r")
  (let* ((endm (make-marker))
         (endm (set-marker endm end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "!\\[\\([^]]*\\)\\](\\([^)]+\\))" endm t)
        (replace-match "<img width=300px src=\"\\2\" alt=\"\\1\" />")))))

(defun dan/jira-link-to-markdown (beg end)
  "Convert all JIRA links in region to markdown links."
  (interactive "r")
  (let* ((endm (make-marker))
         (endm (set-marker endm end)))
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "\\(https?://.+/\\([A-Z]+-[0-9]+\\)\\)" endm t)
        (replace-match "[\\2](\\1)")))))

(defun dan/org-table-to-markdown ()
  (interactive)
  (save-excursion
    (replace-regexp "-\\+-" "-|-" nil (point-min) (point-max))))


;;; Chrome
(defun dan/chrome-autoreload (tab-name)
  (interactive "sChrome tab name: ")
  (let* ((chrome-cli-output
          (shell-command-to-string
           (format "chrome-cli list tabs | grep '%s'" tab-name)))
         (tab-id (or (and (string-match "^\[[0-9]+:\\([0-9]+\\)\]" chrome-cli-output)
                          (match-string 1 chrome-cli-output))
                     (error "Can't parse chrome-cli output: %s" chrome-cli-output)))
         (cmd (format "sleep 0.7 && chrome-cli reload -t %s" tab-id)))
    (message cmd)
    (dan/set-after-save-command cmd)))


;;; Pelican
(defun dan/pelican-autoreload ()
  (interactive)
  (let* ((title (or (dan/pelican-get-title)
                    (error "Can't find title")))
         (chrome-cli-output
          (shell-command-to-string
           (format "chrome-cli list tabs | grep '%s'" title)))
         (tab-id (or (and (string-match "^\[[0-9]+:\\([0-9]+\\)\]" chrome-cli-output)
                          (match-string 1 chrome-cli-output))
                     (error "Can't parse chrome-cli output: %s" chrome-cli-output)))
         (cmd (format "sleep 0.7 && chrome-cli reload -t %s" tab-id)))
    (message cmd)
    (dan/set-after-save-command cmd)))


(defun dan/pelican-get-title ()
  (save-excursion
    (goto-char (point-min))
    (and (looking-at "Title: +\\(.+\\) *")
         (match-string 1))))


;;; Python

(defun dan/insert-ipdb-set-trace (&optional traceback)
  (interactive "P")
  ;; (indent-for-tab-command)
  (let ((debugger "ipdb")) ;; pudb
    (insert
     (format
      (if traceback
          "import traceback ; import %s ; print(traceback.format_exc()) ; %s.set_trace()"
        (if t
            "import %s ; %s.set_trace()"
          "import IPython; IPython.embed(banner1='')"))
      debugger debugger))))


(setenv "WORKON_HOME" "~/tmp/virtualenvs")  ;; FIXME

(defun dan/python-set-virtualenv (path)
  (interactive (list (read-directory-name "" (getenv "WORKON_HOME"))))
  (unless (file-exists-p path)
    (error "Invalid path: %s" path))
  (let ((flake8 (expand-file-name "bin/flake8" path)))
    (when (file-exists-p flake8)
      (set (make-variable-buffer-local 'flycheck-python-flake8-executable)
           flake8)))
  (set (make-variable-buffer-local 'python-shell-virtualenv-root)
       path))

(defun dan/python-django-shell-plus ()
  (interactive)
  (let ((cmd (format "%s/bin/python %s/counsyl/product/manage.py shell_plus"
                     (directory-file-name python-shell-virtualenv-root)
                     (directory-file-name (dan/git-get-git-dir)))))
    (message cmd)
    (run-python cmd t t)))

(defun dan/ipython-shell ()
  (interactive)
  (run-python
   (format "%s/bin/ipython"
           (directory-file-name python-shell-virtualenv-root)) t t))

(defun dan/jupyter-console (&optional ask-kernel)
  (interactive "P")
  (let* ((jupyter "/Users/dan/tmp/virtualenvs/jupyter-dev/bin/jupyter")
         (kernel (when ask-kernel (read-from-minibuffer "kernel:")))
         (cmd (format "%s console --existing %s" jupyter (or kernel ""))))
    (message cmd)
    (run-python cmd)))


(defun dan/python-cd-site-packages (&optional python3)
  (interactive "P")
  (if (or (null python-shell-virtualenv-root)
          (not (file-exists-p python-shell-virtualenv-root)))
      (call-interactively 'dan/python-set-virtualenv))
  (dired
   (concat
    (file-name-as-directory python-shell-virtualenv-root)
    (format "lib/python%s/site-packages/" (if python3 "3.5" "2.7")))))


(defvar dan/python-shell-function #'run-python)

(defun dan/python-shell-send-buffer (&optional restart-p)
  (interactive "P")
  (when restart-p
    (kill-buffer
     (process-buffer
      (python-shell-get-process-or-error "This buffer has no python process"))))
  (funcall dan/python-shell-function)
  (while (not (python-shell-get-process)) (sleep-for 0.5))
  (python-shell-send-buffer)
  (python-shell-switch-to-shell))

(defun dan/python-shell-send-string (string)
  (while (not (python-shell-get-process)) (sleep-for 0.5))
  (python-shell-send-string string))

(when nil
  ;; What was I doing here?
  (defun dan/python-cd-site-packages ()
    (interactive)
    (if (null python-shell-virtualenv-root)
        (call-interactively 'dan/python-set-virtualenv))
    (cl-flet ((make-site-package python-version
                                 (concat
                                  (file-name-as-directory python-shell-virtualenv-root)
                                  "lib/python2.7/site-packages/"))))))


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

(defun dan/python-current-defun-name ()
  (interactive)
  (save-excursion
    (end-of-line)
    (let* ((get-name (lambda ()
                       (beginning-of-defun)
                       (and (looking-at python-nav-beginning-of-defun-regexp)
                            (match-string 1))))
           (names `(,(funcall get-name)))
           name)
      (when (looking-at "[ \t]")
        (while (looking-at "[ \t]")
          (setq name (funcall get-name)))
          (push name names))
      ;; (push (dan/python-current-module-name) names)
      (setq name (mapconcat #'identity names "."))
      (dan/save-value-to-kill-ring name)
      (message name))))

(defun dan/python-current-module-name ()
  (replace-regexp-in-string
   "\.py$" ""
   (file-name-nondirectory (buffer-file-name))))

(defun dan/python-where-am-i (&optional arg)
  (interactive "P")
  (message
   (dan/save-value-to-kill-ring
    (let ((module (replace-regexp-in-string
                   "\.__init__\\(.py\\)?" ""
                   (replace-regexp-in-string
                    "/" "."
                    (replace-regexp-in-string
                     (concat "^" (dan/git-get-git-dir)) ""
                     (replace-regexp-in-string
                      "\.py$" ""
                      (file-truename (buffer-file-name)))))))
          (def (dan/python-current-defun-name)))
      (if (not arg)
          (format "from %s import %s" module def)
        (format "%s:%s" module def))))))

(defun dan/python-bookmark-set ()
  (interactive)
  (let ((name (dan/python-current-defun-name)))
    (while (bookmark-delete name))
    (bookmark-set name)))

;; Redefine an emacs function to get multiple buffers per dedicated process.

(require 'python)
(defun dan/python-shell-dedicated-process-identifier ()
  (projectile-project-name))

(defun python-shell-get-process-name (dedicated)
  "Calculate the appropriate process name for inferior Python process.
If DEDICATED is t and the variable `buffer-file-name' is non-nil
returns a string with the form
`python-shell-buffer-name'[variable `buffer-file-name'] else
returns the value of `python-shell-buffer-name'."
  (let ((process-name
         (if dedicated
             ;; (format "%s[%s]" python-shell-buffer-name buffer-file-name)
             (format "%s[%s]" python-shell-buffer-name
                     (dan/python-shell-dedicated-process-identifier))
           (format "%s" python-shell-buffer-name))))
    process-name))

(defun dan/python-misc-file (&optional file)
  (interactive (list
                (read-file-name "File name: " dan/python-misc-dir)))
  (with-temp-buffer
    (insert "#!/usr/bin/env python3\n\n")
    (write-file file t)
    (executable-chmod))
  (find-file file)
  (goto-char (point-max)))

;;; Comint

(defun dan/comint-clear-buffer (&optional arg)
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))


;;; Projectile

(defvar dan/projectile-root-parent-directories
  '("site-packages"
    "elpa")
  "List of parent directory names identifying a projectile project.
If a directory is found to have a parent directory with one of
these names, then the directory (not the parent!) is identified
as a projectile project root. For example, python programmers
might add the string \"site-packages\" to this list, in order
that code directories contained in a site-packages/ directory are
identified as projectile project roots")

(defun dan/projectile-root-by-parent-directory (dir &optional list)
  "Identify a project root in DIR by searching for parent directory in LIST.
If LIST is nil use `projectile-project-root-parent-directories'"
  (projectile-locate-dominating-file
   dir
   (lambda (dir)
     (member (file-name-nondirectory (projectile-parent dir))
             dan/projectile-root-parent-directories))))

(defun dan/projectile-root-default (dir)
  "Use DIR as current project root"
  dir)

(defun dan/projectile-root-custom-hash-key (dir)
  (format "dan/custom-%s" dir))

(defun dan/projectile-root-custom (dir)
  (gethash (dan/projectile-root-custom-hash-key dir)
           projectile-project-root-cache))

(defun dan/projectile-set-project-root (dir)
  (interactive "DProject root directory: ")
  (let ((cache-key ))
    (puthash (dan/projectile-root-custom-hash-key default-directory)
             dir
             projectile-project-root-cache)))

(defun dan/project-scratch-buffer ()
  (interactive)
  (find-file "/Users/dan/src/counsyl/misc/GEN-341-evidence-from-annotations.org"))


(defun dan/projectile-add-to-project ()
  (interactive)
  (if (buffer-modified-p) (error "Save buffer first."))
  (let* ((this-file buffer-file-name)
         (project
          (projectile-completing-read
           "Add to project: "
           (projectile-relevant-known-projects)))
         (link-in-project
          (expand-file-name
           (format "%s/tmp/%s" project
                   (file-name-nondirectory (buffer-file-name))))))
    (make-symbolic-link this-file link-in-project t)
    (kill-buffer (current-buffer))
    (find-file link-in-project)
    (message link-in-project)))


;;; Helm
(setq dan/projectile-ignored-files '("*.sql" "*.wsdl" "*.js" "*.css" "*.pdf"))

(setq helm-grep-ignored-files (append helm-grep-ignored-files dan/projectile-ignored-files))
(setq grep-find-ignored-files (append grep-find-ignored-files dan/projectile-ignored-files))

;; (setq helm-grep-git-grep-command "git --no-pager grep -n%cH --color=always --exclude-standard --no-index --full-name -e %p -- %f")
(setq helm-grep-git-grep-command
      (format "%s './*' %s"
              helm-grep-git-grep-command
              (mapconcat (lambda (s) (format "':!%s'" s)) dan/projectile-ignored-files " ")))

(defun dan/helm-projectile-switch-project (&optional arg)
  (interactive "P")
  (if arg (projectile-switch-project)
    (let ((projectile-switch-project-action 'projectile-switch-to-buffer))
      (projectile-switch-project))))

(defun dan/helm-projectile-grep-thing-at-point (&optional search-for-definition)
  (interactive "P")
  (if search-for-definition
      (search-files-thing-at-point 'search-for-definition)
    (if t
        (let ((helm-projectile-set-input-automatically t)
              (grep-find-ignored-files
               (append grep-find-ignored-files dan/projectile-ignored-files)))
          (helm-projectile-grep))
      (counsel-git-grep nil (thing-at-point 'symbol)))))

(defun dan/helm-projectile-grep-no-input (&optional dir)
  "Copied from helm-projectile-grep, disabling `helm-projectile-set-input-automatically'."
  (interactive)
  (funcall 'run-with-timer 0.01 nil
           (lambda (dir)
             (let ((project-root (or dir (projectile-project-root)
                                     (error "You're not in a project")))
                   (helm-projectile-set-input-automatically nil)
                   (grep-find-ignored-files
                    (append grep-find-ignored-files dan/projectile-ignored-files)))
               (helm-projectile-grep-or-ack project-root))) dir))


(defun dan/helm-swoop-thing-at-point ()
  (interactive)
  (let ((helm-swoop-pre-input-function (lambda () (thing-at-point 'symbol))))
    (helm-swoop)))

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
    (cond
     ((string-match "\\([^@]+\\)@\\([^:]+\\):\\([^.]+\\)\\(.git\\)?" remote-uri)
      (let ((protocol (match-string 1 remote-uri))
            (hostname (match-string 2 remote-uri))
            (repo (match-string 3 remote-uri)))
        (format "https://%s/%s" hostname repo)))
     (t (error "Failed to parse URI: %s" remote-uri)))))

(defun dan/save-value-to-kill-ring (&optional sexp)
  (interactive "XExpression to evaluate and save to kill-ring: ")
  (with-temp-buffer
    (let ((string (format "%s" sexp)))
      (insert string)
      (kill-ring-save (point-min) (point-max))
      string)))
