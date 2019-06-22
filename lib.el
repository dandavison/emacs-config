;;; Etc

(defun dan/message-buffer-goto-end-of-buffer (&rest args)
  (let* ((win (get-buffer-window "*Messages*")))
    (if win
        (set-window-point
         win (with-current-buffer (window-buffer win) (point-max))))))


(defun dan/keyboard-quit ()
  (interactive)
  (keyboard-quit)
  (with-current-buffer (window-buffer (minibuffer-window)) (keyboard-quit)))


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
      ((not arg)
       'counsel-recentf)
      ((and (equal arg '(4))
            (projectile-project-p))
       'projectile-find-file)
      ((equal arg '(4)) 'counsel-find-file)
      ((equal arg '(16))
       'find-file)))))

(defun dan/set-exec-path-from-shell (&optional pathvar)
  (interactive)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (shell-command-to-string
           (format "/bin/bash -c '. ~/src/shell-config/path.sh && echo -n $%s'" pathvar))))
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
         (fn (and bfn (file-name-nondirectory bfn))))
    (if (not bfn)
        (message bn)
      (add-text-properties
       (- (length bfn) (length fn)) (length bfn) (list 'face 'org-warning) bfn)
      (dan/save-value-to-kill-ring bfn)
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
         (dan/git-get-git-dir))))))

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

(defun dan/log (format-string value &optional formatter)
  (message (format format-string (funcall (or formatter 'identity) value)))
  value)


(defun dan/message (&rest args)
  (apply 'message args)
  (message nil))

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
(defun dan/company-indent-or-complete ()
  (interactive)
  (if (and (or (looking-at "[ \n)]")
               (equal (point) (point-max)))
           (not (looking-back " "))
           (> (current-column) 0))
      (company-complete)
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

(defun dan/goto-emacs-config (&optional arg)
  (interactive "P")
  (if arg (find-file "~/src/emacs-config/lib.el") (dan/goto-use-package)))

(defun dan/find-dot-emacs (&optional arg)
  (interactive "P")
  (find-file (if arg "~/src/emacs-config/lib.el" (file-chase-links "~/.emacs.d/init.el"))))

(defun dan/switch-to-messages-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*")
  (goto-char (point-max)))

(defun dan/goto-use-package ()
  (interactive)
  (dan/find-dot-emacs)
  (swiper "(use-package "))

(defun dan/projects-file ()
  (interactive)
  (find-file (file-chase-links "~/dandavison7@gmail.com/Projects/projects.org")))

(defun dan/info-file ()
  (interactive)
  (find-file (file-chase-links "~/dandavison7@gmail.com/Legal/info.txt")))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun dan/python-infer-project-name ()
  "The name of the current python project.

Suppose the name is $name. The following statements are true:
1. The project is held in a git repository at a path like .../$name/.git.
2. The full path is known to projectile.
3. The project virtualenv is a directory also named $name. Its
   parent directory is python-environment-directory."
  (let* ((file-name (buffer-file-name)))
    (if (string-match
         (format "%s/\\([^/]+\\)/" (directory-file-name python-environment-directory))
         file-name)
        ;; We're in the virtualenv.
        (match-string 1 file-name)
      ;; We're in the project directory. Note: this will give
      ;; an incorrect answer if we are in a projectile project
      ;; directory nested within the desired project directory.
      (and (projectile-project-p) (projectile-project-name)))))

(defun dan/python-infer-virtualenv (&optional project-name)
  "Infer absolute path to python virtualenv for current buffer."
  (let* ((project-name (or project-name (dan/python-infer-project-name)))
         (virtualenv (and project-name
                          (f-join python-environment-directory project-name))))
    (and virtualenv
         (f-directory-p virtualenv)
         (file-name-as-directory virtualenv))))

(defun dan/python-infer-project-root (&optional project-name)
  "Infer absolute path to project root.

The project root is the place where you might find tox.ini, setup.py, Makefile, etc."
  (let* ((project-name (or project-name (dan/python-infer-project-name)))
         (project-root (and project-name
                            (-first (lambda (path) (equal (f-filename path) project-name))
                                    (mapcar 'expand-file-name projectile-known-projects)))))
    (and project-root
         (f-directory-p project-root)
         (file-name-as-directory project-root))))

(defun dan/python-cd-site-packages ()
  (interactive)
  (let ((virtualenv (dan/python-infer-virtualenv)))
    (dired
     (-first
      'f-directory-p
      (mapcar
       (lambda (version) (f-join dan/python-virtualenv (format "lib/python%s/site-packages" version)))
       '("3.6" "3.7" "2.7"))))))

(defun dan/python-show-buffer-config ()
  (interactive)
  (with-temp-buffer-window
   "*Python Buffer Config*"
   nil nil
   (princ
    (format
     "Python buffer-local config\n--------------------------\n\n%s\n"
     (mapconcat (lambda (sym) (format "%-40s %s" (symbol-name sym) (eval sym)))
                dan/python-buffer-config-keys
                "\n")))))

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
  ;; TODO: use jedi for this?
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


;;; Company

(require 'cl-lib)

(defun dan/company-python-django-model-manager-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (message "%s %s %s" command arg ignored)

  (cl-case command
    (interactive (company-begin-backend 'dan/company-python-django-model-manager-backend))
    (prefix (and (eq major-mode 'python-mode)
                 (looking-back "objects\\.")
                 (match-string 0)))
    (candidates
     (mapcar (lambda (method) (concat "objects." method))
             '("aggregate"
               "all"
               "annotate"
               "auto_created"
               "bulk_create"
               "check"
               "complex_filter"
               "contribute_to_class"
               "count"
               "create"
               "creation_counter"
               "dates"
               "datetimes"
               "db"
               "db_manager"
               "deconstruct"
               "defer"
               "difference"
               "distinct"
               "earliest"
               "exclude"
               "exists"
               "extra"
               "filter"
               "first"
               "from_queryset"
               "get"
               "get_or_create"
               "get_queryset"
               "intersection"
               "in_bulk"
               "iterator"
               "last"
               "latest"
               "none"
               "only"
               "order_by"
               "prefetch_related"
               "raw"
               "reverse"
               "select_for_update"
               "select_related"
               "union"
               "update"
               "update_or_create"
               "use_in_migrations"
               "using"
               "values"
               "values_list")))))




;;; Counsel
(defun counsel-git-grep-cmd-function-with-pathspec (str)
  "If there are multiple input patterns, interpret the first as a
git pathspec constraining the files searched by `git grep`.
See `man gitglossary` or
https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefpathspecapathspec

A simple example is:
**/somedir/* regex1 regex2 ...

A more complex example is:
':(exclude)*/tests/*' regex1 regex2 ...
"
  (let* ((parts (split-string str " " t))
         (separator-pos (position ":" parts :test #'equal))
         (before-sep (subseq parts 0 (or separator-pos 0)))
         (after-sep (subseq parts (if separator-pos (1+ separator-pos) 0)))
         (pathspec (string-join before-sep " "))
         (regex (ivy--regex (string-join after-sep " ") t))
         (git-grep-cmd-without-pathspec (format counsel-git-grep-cmd regex)))
    (setq ivy--old-re regex)
    (if (> (length pathspec) 0)
        (format "%s \"%s\"" git-grep-cmd-without-pathspec pathspec)
      git-grep-cmd-without-pathspec)))

(setq counsel-git-grep-cmd-function #'counsel-git-grep-cmd-function-with-pathspec)

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


(defun dan/grep ()
  (interactive)
  (call-interactively 'counsel-git-grep))


(defun dan/grep-thing-at-point ()
  (interactive)
  (counsel-git-grep nil (ivy-thing-at-point)))


(defun dan/goto-definition (&optional arg)
  (interactive "P")
  (if (equal major-mode 'python-mode)
      (call-interactively 'jedi:goto-definition)
    (find-function (or (and (not arg) (function-called-at-point))
                       (intern (completing-read
                                "Function: "
                                #'help--symbol-completion-table
                                (lambda (f) (or (fboundp f) (get f 'function-documentation)))
                                t nil nil nil))))))




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
