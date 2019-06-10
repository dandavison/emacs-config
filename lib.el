;;; Etc

(defun dan/find-dot-emacs (&optional arg)
  (interactive "P")
  (find-file (if arg "~/src/emacs-config/lib.el" (file-chase-links "~/.emacs.d/init.el"))))

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
       'counsel-projectile-switch-to-buffer
     'switch-to-buffer)))

(defun dan/find-file (&optional arg)
  (interactive "P")
  (let ((projectile-globally-ignored-file-suffixes
         (append projectile-globally-ignored-file-suffixes '("png" "pdf"))))
    (call-interactively
     (cond
      ((and (not arg)
            (projectile-project-p))
       'projectile-find-file) ;; counsel- version seemed to have hopeless performance
      ((not arg) 'ido-find-file)
      ((equal arg '(4))
       'counsel-recentf)
      ((equal arg '(16))
       'counsel-find-file)))))

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
          (or "/Users/dan/.pyenv/shims:/Users/dan/bin:/Users/dan/src/emacs-config/bin:/Users/dan/src/misc:/Users/dan/go/bin:/usr/local/opt/postgresql@9.6/bin:/usr/local/opt/coreutils/libexec/gnubin:/usr/local/bin:/usr/local/texlive/2018/bin/x86_64-darwin:/Users/dan/.pyenv/shims:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin:/Users/dan/src/counsyl/bin:/Users/dan/src/counsyl/bin"
              (shell-command-to-string
               (format "/bin/bash -c '. ~/.bashrc && echo -n $%s'" pathvar)))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))

(defun dan/where-am-i (&optional arg)
  (interactive "P")
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'django-mode))
      (dan/python-where-am-i arg)
    (dan/show-buffer-file-name)))

(defun counsyl/current-website-repo () nil)

(defun dan/show-buffer-file-name ()
  (interactive)
  (let* ((bn (buffer-name (current-buffer)))
         (bfn (copy-sequence (buffer-file-name)))
         (fn (file-name-nondirectory bfn)))
    (add-text-properties
     (- (length bfn) (length fn)) (length bfn) (list 'face 'org-warning) bfn)
    (when bfn (dan/save-value-to-kill-ring bfn))
    (let ((website (counsyl/current-website-repo)))
      (message
       "%s(%s) %s %s"
       (if website (dan/add-face-to-string
                    (format "website-%s" website) 'org-warning)
         "")
       (if website (dan/git-get-branch) "")
       (if bfn
           (replace-regexp-in-string
            (concat "^" (dan/git-get-git-dir)) "" bfn)
         "")
       (dan/git-get-git-dir)))))

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

(defvar dan/narrow-to-region-original-mode nil)

(defun dan/narrow-to-region (&optional arg)
  (interactive)
  (when (not (region-active-p))
    (push-mark (point-at-eol)))
  (call-interactively 'narrow-to-region)
  (deactivate-mark)
  (when arg
    (set (make-variable-buffer-local 'dan/narrow-to-region-original-mode)
         major-mode)
    (let ((mode (intern
                 (ido-completing-read
                  "mode: "
                  `(,(symbol-name major-mode) "python-mode" "sql-mode")))))
      (unless (eq major-mode mode) (funcall mode)))))

(defun dan/widen ()
  (interactive)
  (call-interactively 'widen)
  (when dan/narrow-to-region-original-mode
    (unless (eq major-mode dan/narrow-to-region-original-mode)
      (funcall dan/narrow-to-region-original-mode))))

(defun dan/pop-to-mark-command ()
  "Jump to mark, and pop a new position for mark off the ring.
\(Does not affect global mark ring)."
  (interactive)
  (if (null (mark t))
      (user-error "No mark set in this buffer")
    (if (= (point) (mark t))
        (message "Mark popped"))
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; dan added this
    (while (< (abs (- (line-number-at-pos (mark t))
                      (line-number-at-pos (point))))
              10)
      (pop-mark))
    ;;;;;;;;;;;;;;;;;;;;;;;;;
    (goto-char (mark t))
    (pop-mark)
    (dan/pulse-momentary-highlight-current-line)))

(defun dan/add-face-to-string (string face)
  "Destructive. `face' must be a symbol"
  (add-text-properties 0 (length string) (list 'face face) string)
  string)

(defun dan/compile-jfdi ()
  (interactive)
  (let ((default-directory (locate-dominating-file "." "Makefile"))
        (compile-command "make"))
    (setq-default compilation-directory default-directory)
    (compilation-start compile-command)))

(defun dan/compile-on-save (&optional arg)
  (interactive "P")
  (if arg
      (remove-hook 'after-save-hook 'dan/compile-jfdi t)
    (add-hook 'after-save-hook 'dan/compile-jfdi nil t)))

;;; Indentation

(defun dan/indent-shift-left (&rest args)
  (interactive)
  (let ((python-indent-offset 2))
    (call-interactively 'python-indent-shift-left)))

(defun dan/indent-shift-right (&rest args)
  (interactive)
  (let ((python-indent-offset 2))
    (call-interactively 'python-indent-shift-right)))

(defun dan/indent-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'indent-region)
    (save-excursion
      (indent-region (point-min) (point-max)))))

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

(defun dan/show-shell-output-buffer ()
  (interactive)
  (let ((buf (get-buffer-create "*Async Shell Command*"))
        (small-window-lines -4))
    (when buf
      (delete-other-windows)
      (set-window-buffer
       (split-window-vertically small-window-lines) buf)
      (with-current-buffer buf (goto-char (point-min))))))

(defun dan/watch (&optional arg)
  (interactive "P")
  (dan/set-after-save-command "make")
  (dan/show-shell-output-buffer))

(defun dan/watch-mathematics (&optional arg)
  (interactive "P")
  (let ((pdf
         (format "%s.pdf"
                 (file-name-sans-extension
                  (file-name-nondirectory (buffer-file-name))))))
    (dan/set-after-save-command
     (format "make %s > /dev/null" (if (file-exists-p pdf) pdf "mathematics.pdf"))))
  (dan/show-shell-output-buffer))

(defun dan/save-even-if-not-modified ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))


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


;;; Hot files
(defun dan/projects-file ()
  (interactive)
  (find-file (file-chase-links "~/GoogleDrive/Projects/projects.org")))

(defun dan/info-file ()
  (interactive)
  (find-file (file-chase-links "~/GoogleDrive/Legal/info.txt")))

(defun dan/alias-file ()
  (interactive)
  (find-file (file-chase-links "~/src/shell-config/alias.sh")))


;;; Search

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

(defun dan/pulse-momentary-highlight-current-line ()
  (pulse-momentary-highlight-one-line (point) 'diff-refine-changed))


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
(defun dan/set-up-outline-minor-mode (outline-regexp &optional activate)
  (set (make-local-variable 'outline-regexp) outline-regexp)
  (when (or t activate)
    (outline-minor-mode t)
    (when (eq (point) (point-min))
      (org-overview)
      (org-content))))


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
    (backward-char (length close))))

(defun dan/paired-dollar ()
  (interactive)
  (dan/paired-character "$" "$"))

(defun dan/paired-paren (&optional arg)
  (interactive "P")
  (cond
   ((not arg)
    (dan/paired-character "(" ")"))
   ((equal arg '(4))
    (dan/paired-character "\\(" "\\)"))
   ((equal arg '(16))
    (dan/paired-character "\\Big(" "\\Big)"))))

(defun dan/paired-brace (&optional arg)
  (interactive "P")
  (if arg
      (dan/paired-character "\\{" "\\}")
    (dan/paired-character "{" "}")))

(defun dan/paired-pipe (&optional arg)
  (interactive "P")
  (cond
   ((not arg)
    (dan/paired-character "|" "|"))
   ((equal arg '(4))
    (dan/paired-character "\\|" "\\|"))
   ((equal arg '(16))
    (dan/paired-character "\\Big|" "\\Big|"))
   ((equal arg '(64))
    (dan/paired-character "\\Bigg|" "\\Bigg|"))))

(defun dan/paired-bracket ()
  (interactive)
  (dan/paired-character "[" "]"))

(defun dan/setup-paired-characters ()
  (interactive)
  (local-set-key "{" 'dan/paired-brace)
  (local-set-key "(" 'dan/paired-paren)
  (local-set-key "$" 'dan/paired-dollar)
  (local-set-key "[" 'dan/paired-bracket)
  (local-set-key "|" 'dan/paired-pipe))

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

(defun dan/key-disabled ()
  (interactive)
  (message "key binding disabled!"))

;;; Images

(defun dan/image-insert-cliboard-image ()
  ;; TODO seems to insert multiple tiled images
  (interactive)
  (let ((file "img.png"))
    (shell-command (format "pngpaste %s && mogrify -resize 400 %s" file file))
    (insert-image-file file)))

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

(defun dan/latex-insert-item-in-align-environment ()
  (let ((copied-line
         (when current-prefix-arg
           (save-excursion (beginning-of-line)
                           (and (looking-at ".+&=\\(.+\\)")
                                (match-string 1))))))
    (latex-indent)
    (insert "&= ")
    (when copied-line (insert copied-line))
    (latex-indent)))


(defun dan/latex-yank-clipboard-image-maybe ()
  (interactive)
  (let ((output-file)
        (temp-file "/tmp/dan--latex-yank-clipboard-image-maybe.png"))
    (with-temp-buffer
      (let ((exit-status (call-process "pngpaste" nil `(:file ,temp-file) nil "-")))
        (if (= exit-status 0)
            (progn
              (setq output-file
                    (read-file-name "File to save image: " (format "%s/img" default-directory)))
              (when (file-exists-p output-file) (error "File exists: %s" output-file))
              (copy-file temp-file output-file t)))))
    (if output-file
        (insert (format "\\begin{mdframed}\n\\includegraphics[width=400pt]{%s}\n\\end{mdframed}"
                        (file-relative-name output-file)))
      (call-interactively 'yank))))


(defun dan/latex-fill-paragraph ()
  (interactive)
  (text-mode)
  (call-interactively 'fill-paragraph)
  (latex-mode))

(defun dan/latex-set-builder-pipe ()
  (interactive)
  (insert "~|~"))

(defun dan/latex-format-region (fmt beg end)
  (let ((end-marker (set-marker (make-marker) end)))
    (goto-char end)
    (insert "}")
    (goto-char beg)
    (insert (format "{\\%s " fmt))
    (goto-char end-marker)
    (forward-char)))

(defun dan/latex-bold (beg end)
  (interactive "r")
  (dan/latex-format-region "bf" beg end))

(defun dan/latex-italic (beg end)
  (interactive "r")
  (dan/latex-format-region "it" beg end))

(defun dan/latex-fixed-width (beg end)
  (interactive "r")
  (dan/latex-format-region "tt" beg end))

(defun dan/latex-definition (beg end)
  (interactive "r")
  (let ((end-marker (set-marker (make-marker) end)))
    (goto-char end)
    (insert "}")
    (goto-char beg)
    (insert "\\defn{")
    (goto-char end-marker)
    (forward-char)))

(defun dan/latex-frac-region ()
  (let ((s (buffer-substring (region-beginning) (region-end))))
    (when (string-match "\\([^/]+\\)/\\([^/]+\\)" s)
      (delete-region (region-beginning) (region-end))
      (insert (format "\\frac{%s}{%s}" (match-string 1 s) (match-string 2 s))))))

;; DNW
;; (defun dan/latex-frac-region (beg end)
;;   (interactive "r")
;;   (save-excursion
;;     (goto-char beg)
;;     (while (re-search-forward "\\([^/]+\\)/\\([^/]+\\)" end t)
;;      (replace-match "\\\\frac{\\1}{\\2}"))))

(defun dan/latex-unfrac-region (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\\\frac{\\([^}]+\\)}{\\([^}]+\\)}" end t)
      (replace-match "\\1/\\2"))))

(defun dan/latex-frac-back ()
  (when (looking-back " \\([^/ ]+\\) */ *\\([^/ ]+\\)")
    (let ((numerator (match-string 1))
          (denominator (match-string 2)))
      (delete-region (match-beginning 0) (match-end 0))
      (insert (format "\\frac{%s}{%s}"  numerator denominator)))))

(defun dan/latex-frac ()
  (interactive)
  (if (region-active-p)
      (dan/latex-frac-region)
    (dan/latex-frac-back)))

(defun dan/latex-frac-or-unfrac (&optional arg)
  (interactive "P")
  (call-interactively
   (if arg 'dan/latex-unfrac-region 'dan/latex-frac)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; latex-focus

(defvar dan/latex-focus-comment-beg "\\begin{comment}  % latex-focus")
(defvar dan/latex-focus-comment-end "\\end{comment}  % latex-focus")
(defvar dan/latex-focus-comment-delimiter-regex
  (format "\\(\\%s\\|\\%s\\)"
          dan/latex-focus-comment-beg
          dan/latex-focus-comment-end))

(defun dan/latex-focus (beg end)
  (interactive "r")
  (assert (region-active-p) "No region selected")
  (assert (not (dan/latex-focused)) "Already focused")
  (dan/latex-focus-insert-comment-delimiters beg end)
  (dan/latex-focus-narrow-to-region))

(defun dan/latex-unfocus ()
  (interactive)
  (goto-char (point-min))
  (widen)
  (assert (dan/latex-focused) "Not focused")
  (dan/latex-focus-remove-comment-delimiters))

(defun dan/latex-focus-get-document-coordinates (beg end)
  (list
   (dan/point-at-bol-after (or (search-backward "\\begin{document}" nil t)
                               (point-min)))
   (dan/point-at-bol-before beg)
   (dan/point-at-bol-after end)
   (dan/point-at-bol-before (or (search-forward "\\end{document}" nil t)
                                (point-max)))))

(defun dan/latex-focus-narrow-to-region ()
  (interactive)
  (narrow-to-region
   (save-excursion
     (goto-char (point-min))
     (dan/point-at-bol-after
      (search-forward dan/latex-focus-comment-end)))
   (save-excursion
     (goto-char (point-max))
     (search-backward dan/latex-focus-comment-beg))))

(defun dan/latex-focused ()
  (save-excursion
    (goto-char (point-min))
    (re-search-forward dan/latex-focus-comment-delimiter-regex nil t)))

(defun dan/latex-focus-insert-comment-delimiters (beg end)
  (interactive "r")
  (save-excursion
    (-let* (((pre-comment-beg
              pre-comment-end
              post-comment-beg
              post-comment-end)
             (dan/latex-focus-get-document-coordinates beg end)))
      (goto-char post-comment-end)
      (insert dan/latex-focus-comment-end "\n")
      (goto-char post-comment-beg)
      (insert dan/latex-focus-comment-beg "\n")
      (goto-char pre-comment-end)
      (insert dan/latex-focus-comment-end "\n")
      (goto-char pre-comment-beg)
      (insert dan/latex-focus-comment-beg "\n"))))

(defun dan/latex-focus-remove-comment-delimiters ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines dan/latex-focus-comment-delimiter-regex)))

(defun dan/point-at-bol-before (pos)
  (save-excursion
    (goto-char pos)
    (point-at-bol 0)))

(defun dan/point-at-bol-after (pos)
  (save-excursion
    (goto-char pos)
    (point-at-bol 2)))

;; end latex-focus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Org
(defun dan/org-babel-execute-non-native-src-block ()
  (interactive)
  (let ((mode major-mode)
        (heading "* \n"))
    (org-narrow-to-block)
    (org-mode)
    ;; hack: org requires a heading in file
    (save-excursion
      (goto-char (point-min))
      (insert heading))
    (forward-char (length heading))
    (org-babel-execute-src-block-maybe)
    (save-excursion
      (goto-char (point-min))
      (delete-char (length heading)))
    (widen)
    (funcall mode)))

(defun dan/org-insert-clipboard-image (&optional file)
  (interactive "F")
  (shell-command (concat "pngpaste " file))
  (insert (concat "[[" file "]]"))
  (org-display-inline-images))


(fset 'x
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item '([3 5 104 111 up down] 0 "%d") arg)))


(defun dan/org-html-export-to-html ()
  (kmacro-exec-ring-item '([3 5 104 111 up down] 0 "%d") nil)
  ;;(org-html-export-to-html t)
  )

;; (add-hook 'after-save-hook 'dan/org-html-export-to-html)

;;; Eplot
(defun dan/eplot-region ()
  (interactive)
  (let ((tmp-file "/tmp/eplot.png"))
    (shell-command-on-region (region-beginning) (region-end)
                             (format "eplot -q -P -o %s" tmp-file))
    (insert-image (create-image tmp-file))))

;;; Magit

;; This doesn't seem to work: cl-flet doesn't override name in desired scope?
;; (defun dan/magit-dired-rename ()
;;   (interactive)
;;   (cl-flet ((dired-rename-file (file newname ok-if-already-exists)
;;                                (message "In redefined dired-rename-file")
;;                                (magit-file-rename file newname)))
;;     (call-interactively 'dired-do-rename)))


;; DNW?
;; (defun dan/magit-dired-rename ()
;;   (interactive)
;;   (let* ((file (dired-file-name-at-point))
;;          (newname (read-file-name (format "New name: " file)
;;                                   (expand-file-name (file-name-directory file)))))
;;     (magit-file-rename file newname)))

(defun dan/magit-hide-all-sections ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (while t
        (magit-section-forward)
        (magit-section-hide (magit-current-section))))))

(defun dan/magit-diff (&optional arg)
  (interactive "P")
  (if arg (progn (execute-kbd-macro 'dan/magit-diff-master)
                 (magit-section-cycle-global)
                 (magit-section-cycle-global))
    (magit-show-commit "HEAD")))

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


;;; iterm2-dwim
(defun dan/iterm2-dwim ()
  (interactive)
  (let ((path (thing-at-point 'filename)))
    (when path
      (let ((display-buffer-alist
             `(("*Async Shell Command*" . (display-buffer-no-window . nil)))))
        (async-shell-command
         (format "/usr/local/bin/iterm2-dwim '%s' '%s'"
                 path (buffer-substring (point) (point-at-eol))))))))


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

(defun dan/blacken ()
  (interactive)
  (unless (boundp 'dan/blacken-this-file)
    (set (make-local-variable 'dan/blacken-this-file)
         (equal (ido-completing-read "Blacken this file?: " '("no" "yes")) "yes")))
  (when dan/blacken-this-file
    (let ((cmd (format "~/bin/black -l 99 %s" (buffer-file-name))))
      (message cmd)
      (save-window-excursion
        (async-shell-command cmd)))))

(defun dan/insert-ipdb-set-trace ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()")
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


(defun dan/python-insert-ipdb-set-trace (&optional traceback)
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
  (let* ((jupyter "jupyter")
         (kernel (when ask-kernel (read-from-minibuffer "kernel:")))
         (cmd (format "%s console --existing %s" jupyter (or kernel ""))))
    (message cmd)
    (run-python cmd)))


(defun dan/python-cd-site-packages (&optional python2)
  (interactive "P")
  (if (or (null python-shell-virtualenv-root)
          (not (file-exists-p python-shell-virtualenv-root)))
      (call-interactively 'dan/python-set-virtualenv))
  (let* ((directory (concat
                     (file-name-as-directory python-shell-virtualenv-root)
                     (format "lib/python%s/site-packages/" (if python2 "2.7" "3.6"))))
         (package-directory
          (helm
           :sources
           (helm-build-sync-source "test"
             :candidates (dan/helm-directory-candidates directory)
             :fuzzy-match t)
           :buffer "*Site Packages*")))
    (message "dan/python-cd-site-packages: %s" package-directory)
    (dired package-directory)))


(defun dan/helm-directory-candidates (directory)
  "Return alist of directory items
  Each element is (formatted-item . item)"
  (mapcar
   (lambda (file)
     `(,(file-name-nondirectory file)
       .
       ,file))
   (directory-files directory 'full)))


(defun dan/python-site-packages ()
  "Open dired buffer on an installed python packages"
  (interactive "P")
  (let ((facet (if (not arg) (facet-current-facet)
                 (helm :sources
                       `((name . "Facets")
                         (candidates . ,(facet-candidates-list)))))))
    (dired (expand-file-name facet (facet-facets-directory)))))


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

(defun dan/python-shell-eval ()
  (interactive)
  (if (region-active-p)
      (progn
        (call-interactively 'python-shell-send-region)
        (deactivate-mark))
    (python-shell-send-buffer))
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
                                  "lib/python3.6/site-packages/"))))))


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

(defun dan/file-or-directory-name ()
  (if (eq major-mode 'dired-mode)
      (dired-current-directory)
    (buffer-file-name)))

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
                      (file-truename (dan/file-or-directory-name)))))))
          (def (dan/python-current-defun-name)))

      (cond
       ((equal arg '(4))
        (format "%s:%s" module def))
       ((equal arg '(16))
        def)
       (t (format "from %s import %s" module def)))))))

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

;;; SQL
(defun sqlparse-region (beg end)
  (interactive "r")
  (shell-command-on-region
   beg end
   "python -c 'import sys, sqlparse; print(sqlparse.format(sys.stdin.read(), reindent=True))'"
   t t))

;;; Javascript
(defun dan/set-after-save-command-for-t ()
  (interactive)
  (dan/set-after-save-command "export PATH=/Users/dan/.nvm/versions/node/v10.11.0/bin:$PATH && cd /Users/dan/src/t && make js"))

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


(defun dan/grep-thing-at-point (&optional arg)
  (interactive "P")
  (if (equal arg '(16))
      (if (equal major-mode 'python-mode)
          (jedi:goto-definition)
        (search-files-thing-at-point 'search-for-definition))
    (let ((counsel-projectile-grep-initial-input (ivy-thing-at-point)))
      (call-interactively 'counsel-projectile-git-grep))))


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


(defun dan/git-get-branch ()
  "Current branch"
  (org-babel-chomp
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))


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
