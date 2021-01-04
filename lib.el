;; -*- lexical-binding: t -*-
;;; Etc

(defun dan/flymake-flycheck-toggle ()
  (interactive)
  (call-interactively (cond (flymake-mode #'flymake-mode)
                            (flycheck-mode #'flycheck-mode)
                            (t #'flymake-mode))))

(defun dan/set-fill-column (n)
  (interactive "nColumn: ")
  (setq dan/fill-column n
        fill-column dan/fill-column
        fci-rule-column dan/fill-column
        fci-rule-color "#A5BAF1"))

(defun dan/message-buffer-goto-end-of-buffer (&rest args)
  (let* ((win (get-buffer-window "*Messages*"))
         (buf (and win (window-buffer win))))
    (and win (not (equal (current-buffer) buf))
         (set-window-point
          win (with-current-buffer buf (point-max))))))

(defun dan/message-buffer-clear ()
  (interactive)
  (let* ((win (get-buffer-window "*Messages*" t))
         (buf (and win (window-buffer win))))
    (with-selected-window win
      (with-current-buffer buf (recenter 1)))))

(defun dan/keyboard-quit ()
  (interactive)
  (keyboard-quit)
  (with-current-buffer (window-buffer (minibuffer-window)) (keyboard-quit)))

(defun dan/describe-properties (prop)
  (interactive "Sproperty: ")
  (let* ((pos (point))
         (ovs (overlays-at pos))
         (text-prop-val (get-text-property pos prop)))
    (if ovs
        (dolist (ov ovs)
          (message "overlay with %S=%S" prop (overlay-get ov prop)))
      (message "<no overlays>"))
    (message "text property %S=%S" prop text-prop-val)
    nil))

(defun dan/describe-properties-in (beg end)
  (interactive "r")
  (let* ((ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (message "overlay with %S=%S" 'face (overlay-get ov 'face)))))

(defun dan/delete-overlays (beg end)
  (interactive "r")
  (mapc #'delete-overlay (overlays-in (point-min) (point-max))))

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
   (cond
    ((equal arg '(4))
     'ivy-switch-buffer)
    ((and (not arg)
          (projectile-project-p))
     'projectile-switch-to-buffer)
    (t 'switch-to-buffer))))

(defun dan/set-exec-path-from-shell (&optional pathvar)
  (interactive)
  (let* ((pathvar (or pathvar "PATH"))
         (path-from-shell
          (shell-command-to-string
           ;; && . ~/src/shell-config/pyenv.sh
           (format "/bin/bash -c '. ~/src/shell-config/path.sh && echo -n $%s'" pathvar))))
    (setenv pathvar path-from-shell)
    (when (string-equal pathvar "PATH")
      (setq exec-path (split-string path-from-shell path-separator)))))

(defun dan/where-am-i (&optional arg)
  (interactive "P")
  (if (or (eq major-mode 'python-mode)
          (eq major-mode 'django-mode))
      (dan/python-where-am-i arg)
    (message (dan/save-value-to-kill-ring (dan/current-defun)))))

(defun dan/current-defun ()
  (save-excursion
    (beginning-of-defun)
    (buffer-substring (point-at-bol) (point-at-eol))))


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

(defun dan/lightning ()
  (interactive)
  (insert-char #x26A1))

(defun dan/bytes-to-hex (bytes)
  (--map (format "#x%02x" it) bytes))

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

(defun dan/indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(define-minor-mode dan/indent-buffer-on-save-mode
  "indent-buffer-on-save mode"
  :lighter " indent"
  :global t
  (cond
   (dan/indent-buffer-on-save-mode
    (add-hook 'before-save-hook #'dan/indent-buffer nil 'local))
   ('deactivate
    (remove-hook 'before-save-hook #'dan/indent-buffer 'local))))

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
     (format "make %s" (if (file-exists-p pdf) pdf "mathematics.pdf"))))
  (dan/show-shell-output-buffer))

(defun dan/save-even-if-not-modified ()
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))


(defun dan/compile ()
  (interactive)
  (setq compilation-finish-functions '(dan/compilers-compilation-finish))
  (compile "cd ~/src/compilers/ && make"))

(defun dan/compilers-compilation-finish (buf status)
  ;; Hack: there seem to be stray = and > characters at the beginning of lines.
  (with-current-buffer buf
    (let ((inhibit-read-only t))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward "^\\(=\\|\\>\\)" nil t)
         (replace-match ""))))))

(defun dan/default-command ()
  (interactive)
  (dan/compile))

;;; Autoformatting

(defun dan/elisp-format-defun ()
  (interactive)
  (mark-defun)
  (elisp-format-region))

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
                 (progn
                   ( (boundp 'dan/delete-trailing-whitespace-answer)
                     (setq-local dan/delete-trailing-whitespace-answer
                                 (yes-or-no-p "Delete trailing whitespace?"))
                     t))
                 (or (not (boundp 'dan/delete-trailing-whitespace-answer))
                     dan/delete-trailing-whitespace-answer)
                 (delete-trailing-whitespace))))))))

(defun dan/delete-trailing-whitespace-do-not ()
  (interactive)
  (setq-local dan/delete-trailing-whitespace-answer nil))


;;; Appearance

(defun dan/theme-load (&optional theme)
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if theme
      (load-theme theme t)
    (call-interactively 'load-theme))
  (dan/set-appearance))

(defun dan/set-appearance ()
  (interactive)
  (set-frame-font "Monaco 14")
  (scroll-bar-mode -1)
  (set-cursor-color "red")
  (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default cursor-type 'bar)
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

(defun dan/get-fontified-strings (face-spec)
  (mapcar (lambda (font)
            (add-face-text-property 0 (length font)
                                    (append `(:font ,(font-spec :name font)) face-spec)
                                    nil font)
            font)
          (x-list-fonts "*")))

(defun dan/ivy-browse-fonts (face-spec)
  ;; E.g. (browse-fonts :height 1.1 :foreground "blue4" :weight 'bold)
  (interactive "xFace plist: ")
  (let ((ivy-height (window-total-height)))
    (ivy-read "" (dan/get-fontified-strings face-spec))))
;;; Dired

(defun dan/dired-no-ask ()
  (interactive)
  (let ((current-filename (buffer-file-name (current-buffer))))
    (dired default-directory)
    (when current-filename (dired-goto-file current-filename))))


;;; Completion
(defun dan/indent-or-complete ()
  (interactive)
  (if (and (not (looking-back " "))
           (> (current-column) 0))
      (company-complete)
    (funcall indent-line-function)))

;;; Eglot

(defun dan/eglot-pyls-map-local-path-to-docker-tramp-path (path)
  (cl-destructuring-bind (container-id)
      (cl-loop for (id name) in (docker-tramp--running-containers)
               if (s-contains-p "_pyls_" name)
               collect id)
    (format "/docker:%s:%s" container-id path)))


(defun dan/eglot-pyls-map-docker-tramp-path-to-local-path (path)
  (and (string-match "/docker:[0-9a-f]+:/home/docker/website/" path)
     (replace-match (expand-file-name "~/src/counsyl/website-4/") t t path)))


(defun dan/eglot-pyls-open-local-path ()
  (interactive)
  (let ((point (point))
        (buf (current-buffer)))
    (find-file (dan/eglot-pyls-map-docker-tramp-path-to-local-path (buffer-file-name)))
    (goto-char point)
    (kill-buffer buf)))


(defun dan/eglot-dashboard ()
  (interactive)
  (select-frame (make-frame))
  (set-frame-name "Eglot Dashboard")
  (cl-flet ((make-buffer-name (source)
                              (format "*EGLOT (delta/rust-mode) %s*" source)))
    (switch-to-buffer (get-buffer (make-buffer-name "events")))
    (dolist (source '("output" "stderr"))
      (split-window-vertically)
      (display-buffer (make-buffer-name source)))
    (balance-windows)))

;;; comment
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

(defun dan/bookmark-dwim (&optional arg)
  (interactive "P")
  (if arg
      (if (eq major-mode 'python-mode)
          (dan/python-bookmark-set)
        (call-interactively 'bookmark-set))
    (call-interactively #'bookmark-bmenu-list)))


;;; Hot files

(defun dan/goto-emacs-config (&optional arg)
  (interactive "P")
  (pcase arg
    (`nil (dan/goto-use-package))
    (`(4) (find-file "~/src/emacs-config/lib.el"))
    (`(16) (find-file "~/src/emacs-config/python.el"))))

(defun dan/goto-dot-emacs (&optional arg)
  (interactive "P")
  (find-file (if arg "~/src/emacs-config/lib.el" (file-chase-links "~/.emacs.d/init.el"))))

(defun dan/display-messages-buffer (&optional arg)
  (interactive "P")
  (if arg (message "\n\n")
    (switch-to-buffer-other-window "*Messages*")
    (goto-char (point-max))))

(defun dan/goto-rustfmt-buffer ()
  (interactive)
  (switch-to-buffer "*rustfmt*"))

(defun dan/goto-use-package ()
  (interactive)
  (dan/goto-dot-emacs)
  (swiper "(use-package "))

(defun dan/goto-projects-file ()
  (interactive)
  (find-file (file-chase-links "~/dandavison7@gmail.com/Projects/projects.org")))

(defun dan/goto-info-file ()
  (interactive)
  (find-file (file-chase-links "~/dandavison7@gmail.com/Legal/info.txt")))

(defun dan/goto-alias-file ()
  (interactive)
  (find-file (file-chase-links "~/src/shell-config/alias.sh")))

(defun dan/goto-gitconfig (&optional arg)
  (interactive "P")
  (find-file (file-chase-links (if arg "~/.gitconfig" "~/src/config/delta/delta.conf"))))

;;; Search

(defun dan/search (beg end)
  (interactive "r")
  (cond
   ((eq major-mode 'rust-mode)
    (ffap (format "https://doc.rust-lang.org/std/?search=%s" (buffer-substring beg end))))
   (t (error "No search backend for major-mode %s" major-mode))))

(defun dan/on-jump-into-buffer ()
  (recenter)
  (outline-show-all) ;; with swiper--ensure-visible this leaves point in the wrong place
  (dan/pulse-momentary-highlight-current-line)
  (when (eq major-mode 'python-mode)
    (dan/python-current-defun-name)))


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
                       (setq dan/highlighted (-union dan/highlighted (list word)))
                       (highlight-regexp word dan/highlight-face))
            (unhighlight (word)
                         (setq dan/highlighted (-difference dan/highlighted (list word)))
                         (unhighlight-regexp word)))
    (let ((word (cond
                 (arg (read-from-minibuffer "Highlight: "))
                 ((region-active-p) (buffer-substring (region-beginning) (region-end)))
                 (t (thing-at-point 'symbol)))))
      (when word
        (if (-contains? dan/highlighted word)
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
(defun dan/set-up-outline-minor-mode (outline-regexp &optional start-folded)
  (set (make-local-variable 'outline-regexp) outline-regexp)
  (outline-minor-mode t)
  (when start-folded
    (when (eq (point) (point-min))
      (org-overview)
      (org-content))))


(defun dan/narrow-to-subtree ()
  (interactive)
  (outline-mark-subtree)
  (call-interactively #'narrow-to-region)
  (deactivate-mark))

;;; Paired characters

;; hack: use one of the many pairing modes
(defun dan/paired-character (open close)
  (let ((cursor-sensor-inhibit '(inserting-paired-characters)))
    (if (region-active-p)
        (progn
          (save-excursion
            (goto-char (region-beginning))
            (insert open))
          (save-excursion
            (goto-char (region-end))
            (insert close)))
      (insert open close)
      (backward-char (length close)))))

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

(defun dan/paired-angle-bracket ()
  (interactive)
  (dan/paired-character "<" ">"))

(defun dan/latex-paired-characters ()
  (interactive)
  (local-set-key "{" 'dan/paired-brace)
  (local-set-key "(" 'dan/paired-paren)
  (local-set-key "$" 'dan/paired-dollar)
  (local-set-key "[" 'dan/paired-bracket))

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

(defun dan/window-configuration (register &optional arg)
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

;;; Elisp

(defun dan/eval-region-or-buffer (beg end)
  (interactive "r")
  (eval-region (or beg (point-min))
               (or end (point-max))))

(define-minor-mode dan/debug-mode
  "Debug-on-error mode"
  :lighter " DEBUG"
  :global t
  (cond
   (dan/debug-mode
    (setq debug-on-error t)
    (if-let ((buffer (get-buffer "*Backtrace*")))
        (kill-buffer buffer)))
   ('deactivate
    (setq debug-on-error nil))))

;;; Open in
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

;; VSCode
(define-minor-mode vscode-mode
  "Emacs - VSCode symbiosis"
  :lighter " VSCode"
  (cond
   (vscode-mode
    (setq dan/open-in-vscode-automatically t))
   ('deactivate
    (setq dan/open-in-vscode-automatically nil))))


(defvar dan/vscode-file-types '(".js" ".ts" ".vue" ".json" ".rs"))
(defun dan/open-in-vscode (&optional file)
  (interactive)
  (when-let* ((file (or file (if (eq major-mode 'dired-mode)
                                 (dired-filename-at-point)
                               buffer-file-name))))
    (if (-any? (lambda (suffix) (s-ends-with? suffix file))
               dan/vscode-file-types)
        (shell-command
         (if (eq major-mode 'dired-mode)
             (format "code %s" file)
           (format "code -g %s:%s:%s"
                   file
                   (1+ (current-line))
                   (1+ (current-column)))))
      (if vscode-mode (message "Not opening in vscode: %s" file)))))

(add-hook 'magit-diff-visit-file-hook (lambda () (and dan/open-in-vscode-automatically
                                                 (dan/open-in-vscode))))

(add-hook 'find-file-hook (lambda () (and dan/open-in-vscode-automatically
                                     (dan/open-in-vscode))))


(defun dan/open-current-buffer-in-vscode-maybe (_)
  (when-let* ((file (buffer-file-name (window-buffer (selected-window)))))
    (and dan/open-in-vscode-automatically
         (dan/open-in-vscode file))))

(add-to-list 'window-buffer-change-functions #'dan/open-current-buffer-in-vscode-maybe)




;;; LaTeX

(defun dan/latex-indent-line-function ()
  (if (and (eq major-mode 'latex-mode)
           (or
            ;; comment often contains the others, so cannot be included in the same regexp.
            (org-between-regexps-p "\\\\begin{comment}"
                                   "\\\\end{comment}" (point-min) (point-max))
            ;; These usually do not contain each other. If that
            ;; assumption is violated, break them all out into
            ;; independent checks.
            (org-between-regexps-p "\\\\begin{\\(verbatim\\|align\\|minted\\)"
                                   "\\\\end{\\(verbatim\\|align\\|minted\\)" (point-min) (point-max))
            (save-excursion (beginning-of-line) (looking-at "%"))))
      'noindent
    ;; TODO: call whatever original value of indent-line-function was
    (LaTeX-indent-line)))


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
  ;; TODO
  ;; 1. If there are macros like newcommand at the beginning of the
  ;;    document, the first begin-comment marker should come after
  ;;    them.
  ;; 2. This needs unit tests.
  (let ((begin-document (search-backward "\\begin{document}" nil t))
        (end-document (search-forward "\\end{document}" nil t)))
    (list
     (or (and begin-document (dan/point-at-bol-after begin-document))
         (point-min))
     (dan/point-at-bol-before beg)
     (dan/point-at-bol-after end)
     (or (and end-document (dan/point-at-bol-before end-document))
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

(defun dan/magit-file-rename (&optional file newname)
  (interactive)
  (let* ((file (or file (magit-read-file "Rename file")))
         (dir (file-name-directory file))
         (newname (read-file-name (format "Rename %s to file: " file)
                                  (and dir (expand-file-name dir)))))
    (magit-file-rename file newname)))

(defun dan/magit-commit ()
  (interactive)
  (let ((commit-message (buffer-string)))
    (let ((with-editor-cancel-message ""))
      (with-editor-cancel nil))
    (with-temp-buffer
      (insert commit-message)
      (goto-char (point-min))
      (delete-matching-lines "^[ \t]*#")
      (magit-run-git-with-input "commit" "-F" "-")))
  (magit-refresh-all))

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
        (replace-match "<table><tr><td><img width=300px src=\"\\2\" alt=\"\\1\" /></td></tr></table>")))))

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
   parent directory is dan/virtualenvs-directory."
  (if-let ((file-name (buffer-file-name)))
      (if (string-match
           (format "%s/\\([^/]+\\)/" (directory-file-name dan/virtualenvs-directory))
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
                          (f-join dan/virtualenvs-directory project-name))))
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

(defmacro dan/python-verify-setup--do-check (check)
  `(when (not (eval ,check))
     (princ (format "check failed: %S\n" ,check))))

(defun dan/python-verify-setup ()
  (interactive)
  (with-temp-buffer-window
   "*Python Buffer Config*"
   nil nil
   (princ
    (format
     "Python buffer-local config\n--------------------------\n\n%s\n"
     (mapconcat (lambda (sym) (format "%-40s %s" (symbol-name sym) (eval sym)))
                dan/python-buffer-config-keys
                "\n")))
   (princ "\n\n")
   (dan/python-verify-setup--do-check '(f-directory? dan/python-virtualenv))
   (dan/python-verify-setup--do-check '(f-directory? dan/python-project-root))
   (dan/python-verify-setup--do-check '(f-executable? flycheck-python-flake8-executable))
   (dan/python-verify-setup--do-check '(f-executable? flycheck-python-mypy-executable))
   (dan/python-verify-setup--do-check '(f-file? flycheck-flake8rc))
   (dan/python-verify-setup--do-check '(f-file? flycheck-python-mypy-ini))
   (dan/python-verify-setup--do-check '(f-directory? python-shell-virtualenv-root))
   (dan/python-verify-setup--do-check '(f-executable? python-shell-interpreter))))

(defun dan/question-mark (&optional arg)
  (interactive "P")
  (if arg
      (insert ??)
    (call-interactively
     (cond ((fboundp 'eglot-help-at-point) #'eglot-help-at-point)
           ((fboundp 'eldoc-doc-buffer) #'eldoc-doc-buffer)
           #'display-local-help))))

(defun dan/eglot-help-at-point ()
  (interactive)
  (eglot-help-at-point)
  (when-let ((diagnostic (get-char-property (point) 'flymake-diagnostic)))
    (save-window-excursion
      (other-window 1) ;; Temporary hack
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert "\n\n")
        (insert (eglot--format-markup (flymake--diag-text diagnostic)))))))


(defun dan/python-black ()
  (interactive)
  (cond

   ((null current-prefix-arg)
    ;; blacken buffer, now and always.
    (call-interactively #'blacken)
    ;; Assume that if you want the buffer blackened once, you want it blackened
    ;; on save. Don't use reformatter.el blacken-on-save-mode because it does
    ;; not display an error on failure to blacken.
    ;; https://github.com/purcell/reformatter.el/issues/12
    ;;(add-hook 'before-save-hook (lambda () (blacken-buffer t)) nil t)
    )

   ;; blacken region
   ((eq current-prefix-arg '(4))
    (shell-command-on-region (region-beginning) (region-end)
                             "black-macchiato --line-length 99" nil 'replace))

   ;; blacken defun
   ((eq current-prefix-arg '(16))
    (save-excursion
      (python-mark-defun)
      (let ((current-prefix-arg '(4)))
        (call-interactively #'dan/python-blacken))))))

(defun dan/python-insert-ipdb-set-trace (&optional traceback)
  (interactive "P")
  ;; (indent-for-tab-command)
  (let ((debugger "pdb")) ;; pudb
    (insert
     (format
      (if traceback
          "import traceback ; import %s ; print(traceback.format_exc()) ; %s.set_trace()"
        (if t
            "__import__('pdb').set_trace()"
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
   " \"\\1\": \\2,\n"
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
      (setq name (mapconcat #'identity names "::"))
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
                    "/" "/"
                    (replace-regexp-in-string
                     (concat "^" (dan/git-get-git-dir)) ""
                     (replace-regexp-in-string
                      "\.py$" "\.py"
                      (file-truename (dan/file-or-directory-name)))))))
          (def (dan/python-current-defun-name)))

      (cond
       ((equal arg '(4))
        (format "%s::%s" module def))
       ((equal arg '(16))
        def)
       (t (format "from %s import %s"
                  (s-replace "/" "." (s-replace-regexp "\.py$" "" module))
                  def)))))))

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

(defun dan/elaenia-lint ()
  (interactive)
  (compile "cd /Users/dan/src/elaenia && PATH=~/tmp/virtualenvs/elaenia/bin:$PATH make lint"))

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

;;; Scheme
(defun dan/sicm ()
  (interactive)
  (run-scheme
   "/Users/dan/src-3p/scmutils-20150821-x86-64-OSX/scmutils/mit-scheme/bin/scheme --library /Users/dan/src/3p/scmutils-20150821-x86-64-OSX/scmutils/mit-scheme/lib"))

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
(defun dan/counsel-rg (&optional arg initial-input)
  (interactive "P")
  (let ((dir (or (and (not arg)
                      (eq major-mode 'python-mode)
                      (boundp 'dan/python-project-root)
                      dan/python-project-root)
                 (and arg default-directory))))
    (counsel-rg initial-input dir)))

(defun dan/counsel-rg-thing-at-point (&optional arg)
  (interactive "P")
  (dan/counsel-rg arg (ivy-thing-at-point)))

(defun dan/find-file (&optional arg)
  (interactive "P")
  (let ((current-prefix-arg nil)
        (default-directory (or (and (boundp 'dan/python-project-root)
                                    dan/python-project-root)
                               default-directory)))
    (call-interactively
     (cond
      ((not arg)
       #'projectile-find-file)
      ((equal arg '(4))
       #'counsel-recentf)
      ((equal arg '(16))
       #'find-file)))))

(defun counsel-git-grep-cmd-with-pathspec-function (str)
  "Git grep with control over file paths searched.

To use, set `counsel-git-grep-cmd-function' equal to this function.

The behaviour is the same as default `counsel-git-grep', but you
may optionally use a single ':' character, surrounded by spaces,
to separate the pathspec (to the left) from the regular
expressions (to the right).

For pathspec syntax see `man gitglossary` or
https://git-scm.com/docs/gitglossary#Documentation/gitglossary.txt-aiddefpathspecapathspec

A simple example is:
**/somedir/* : regex1 regex2 ...

A more complex example is:
**/somedir/* :(exclude)*/tests/* : regex1 regex2 ...
"
  (let* ((cmd "git --no-pager grep --full-name -n --no-color -i -I -e %s")
         (parts (split-string str " " t))
         (separator-pos (position ":" parts :test #'equal))
         (pathspec (subseq parts 0 (or separator-pos 0)))
         (regexes (subseq parts (if separator-pos (1+ separator-pos) 0)))
         (regex (ivy--regex (string-join regexes " ") t))
         (git-grep-cmd-without-pathspec (split-string (format cmd regex) " " t)))
    (setq ivy--old-re regex)
    ;; (when (and current-prefix-arg (not pathspec))
    ;;   ;;(setq pathspec (list (format "%s* %s**/*" default-directory default-directory)))
    ;;   ;; (setq pathspec (list (format "%s*" (file-name-directory (buffer-file-name)))))
    ;;   (setq pathspec (list (format "%s*" default-directory))))
    (append git-grep-cmd-without-pathspec pathspec)))

(setq counsel-git-grep-cmd-function #'counsel-git-grep-cmd-with-pathspec-function)

;;; Projectile

(defun dan/projectile-describe-cache ()
  (interactive)
  (let* ((items (--map `(,it . ,(length (gethash it projectile-projects-cache)))
                       (hash-table-keys projectile-projects-cache)))
         (sorted-items (--sort (> (cdr it) (cdr other)) items)))
    (cl-loop for (key . n) in sorted-items
             do (message "%-60s %d" key n))))

(defun dan/projectile-replace ()
  (interactive)
  (call-interactively #'projectile-replace)
  (projectile-save
   a-project-buffers))

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


(defun dan/goto-definition (&optional arg)
  (interactive "P")
  (if (equal major-mode 'emacs-lisp-mode)
      (find-function (or (and (not arg) (function-called-at-point))
                         (intern (completing-read
                                  "Function: "
                                  #'help--symbol-completion-table
                                  (lambda (f) (or (fboundp f) (get f 'function-documentation)))
                                  t nil nil nil))))
    (xref-find-definitions (thing-at-point 'symbol))
    (when (and (equal major-mode 'python-mode)
               (not (s-contains? "/venv/" (buffer-file-name))))
      (dan/eglot-pyls-open-local-path))))



;;; Xenops

(defun dan/xenops-test-syntax-highlighting ()
  (interactive)
  (unless (or (looking-at (caar (xenops-elements-get 'src :delimiters)))
              (looking-at (caar (xenops-elements-get 'minted :delimiters))))
    (error "No src/minted block at point"))
  (xenops-src-apply-syntax-highlighting))

(defun dan/xenops-test-regexp-replacement ()
  (interactive)
  (unless (looking-at (xenops-text-regexp-replacements-make-regexp))
    (error "Not at regexp replacement target"))
  (xenops-text-regexp-replacement-do))


(defun dan/xenops-reload ()
  (interactive)
  (let (files)
    (dolist (file (directory-files "~/src/xenops/" 'absolute))
      (when (s-ends-with? ".el" file)
        (dan/eval-file-with-defvars file)
        (push (file-name-nondirectory file) files)))
    (message "loaded files: %s" (s-join ", " files))
    (save-window-excursion
      (with-temp-buffer
        (find-file user-init-file)
        (goto-char (point-min))
        (re-search-forward "^(use-package xenops")
        (goto-char (match-beginning 0))
        (eval-defun nil)))))


(defun dan/TeX-region-create ()
  (interactive)
  (assert (region-active-p))
  (let ((file (make-temp-file "dan--tex-region-create")))
    (TeX-region-create file
                       (buffer-substring (region-beginning) (region-end))
                       (buffer-file-name) 0)
    (switch-to-buffer-other-window "*TeX-region-create*")
    (erase-buffer)
    (insert-file-contents file)
    (LaTeX-mode)))

(defun dan/preview-config ()
  (interactive)
  (let ((output
         (format "| | | | %s | | |"
                 (s-join " | "
                         (mapcar (lambda (x) (if (symbolp x) (symbol-name x) x))
                                 `(,preview-image-type
                                   ,preview-dvipng-image-type
                                   ,TeX-PDF-from-DVI))))))
    (kill-new output)
    (message output)))

;;; Utilities

(defun dan/save-buffer (&optional arg)
  (interactive "P")
  (call-interactively
   (if (or (not arg) (not (and (fboundp 'projectile-save-project-buffers)
                           (fboundp 'projectile-project-p)
			               (projectile-project-p))))
       #'save-buffer
     #'projectile-save-project-buffers)))

(defun dan/describe-face-at-point ()
  (interactive)
  (let ((face (or (face-at-point) 'default)))
    (describe-face face)
    (if-let (remapping (cdr (assq face face-remapping-alist)))
        (with-current-buffer "*Help*"
          (goto-char (point-min))
          (let ((inhibit-read-only t))
            (insert (format "Face '%s' is remapped:\n\n%s\n\n\n"
                            face (prin1-to-string remapping))))))))

(defun dan/eval-buffer-with-defvars ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

(defun dan/eval-file-with-defvars (file)
  (with-temp-buffer
    (insert-file-contents file)
    (emacs-lisp-mode)
    (dan/eval-buffer-with-defvars)))

(defmacro dan/alist-update! (alist1 alist2)
  `(setq ,alist1
         (let ((-compare-fn (lambda (x y) (equal (car x) (car y)))))
           (-distinct (-union ,alist2 ,alist1)))))

(defun dan/assoc-delete-all (key alist)
  "Like `assq-delete-all' but using `equal' for comparison"
  (delq nil
        (mapcar (lambda (el) (unless (equal (car el) key) el))
                alist)))

(defun dan/git-get-git-dir ()
  "Root dir of current repo"
  (file-name-as-directory
   (org-babel-chomp
    (shell-command-to-string "git rev-parse --show-toplevel 2>/dev/null"))))

(defun dan/git-get-commit ()
  "Current commit"
  (org-babel-chomp
   (shell-command-to-string "git rev-parse HEAD 2>/dev/null")))


(defun dan/git-get-branch ()
  "Current branch"
  (org-babel-chomp
   (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null")))


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

(defun dan/format-event (event)
  (format "%s %s %s" (event-basic-type event) (car event) (event-modifiers event)))
