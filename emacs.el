;; generated by org-babel-tangle

;;* [[file:~/config/emacs/emacs.org::*Extra%20packages][block-1]]
(add-to-list 'load-path "/usr/local/src/elisp-library")
(require 'htmlize)
(require 'regex-tool)
(require 'unbound)
(require 'zenburn)
(require 'windresize)
(require 'xclip)
(require 'highlight-parentheses)
(highlight-parentheses-mode)
(add-to-list 'load-path "/usr/local/src/elisp-library/magit")
(require 'magit)
;; (require 'bbdb)
;; (bbdb-initialize)
;; (require 'ido)
(add-to-list 'load-path "/usr/local/src/elisp-library/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/usr/local/src/elisp-library/yasnippet/snippets")
(require 'recentf)

(load "R-anything-config")
;; (add-to-list 'load-path "/usr/local/src/elisp-library/icicles")
;; (load "icicles/icicles")
;; (icy-mode t)
;; block-1 ends here


;; * [[file:~/config/emacs/emacs.org::*Load%20config%20files][block-2]]
(load "~/src/config/emacs/vanilla")
(load "org-dan")
(load "outline-dan")
(load "ess-dan")
(load "latex-dan")
;; block-2 ends here


;; * [[file:~/config/emacs/emacs.org::*Minor%20modes][block-3]]
(show-paren-mode t)
(winner-mode t)
(recentf-mode t)
(global-font-lock-mode t)
;; (desktop-save-mode t)
;; block-3 ends here


;; * [[file:~/config/emacs/emacs.org::*Completion][block-4]]
;; ;; Things that I'm not really interested in seeing in emacs
;; ;; (you can still open them explicitly)
(setq dan-ignored-extensions
      '(".html" ".csv" ".ps" ".bst" ".cls" ".sty" ".backup" ".log"
	".fdf" ".spl" ".aux" ".ppt" ".doc" ".xls" ".mp3"))

(mapc (lambda(extension)
	(add-to-list 'completion-ignored-extensions extension))
	dan-ignored-extensions)
(ido-mode t) ;; (iswitchb-mode t)
(setq ido-separator " ")

;; As regexps, these should really have terminal $
(mapc (lambda(extension)
	(add-to-list 'ido-ignore-buffers (regexp-quote extension))
	(add-to-list 'ido-ignore-files (regexp-quote extension)))
      dan-ignored-extensions) 

(add-to-list 'ido-ignore-buffers "\\*") ;; if you want *scratch* or *R* just type it
;; (add-to-list 'ido-ignore-files "^[^.]+$") ;; files must have a . in their name (experimental)
;; block-4 ends here


;; * [[file:~/config/emacs/emacs.org::*Key%20bindings][block-5]]
  (load "~/src/config/emacs/keys-dan")
  ;; I've moved the custom-set-faces call into vanilla.el; don't know how to set the faces otherwise
  
  (defun dan-set-keys ()
    (interactive)
    (mapc (lambda(pair) (global-set-key (car pair) (cdr pair)))
          dan-global-keybindings))
  
  (dan-set-keys)
  (define-key emacs-lisp-mode-map "\C-cd" 'edebug-defun)
  (define-key ctl-x-4-map "t" 'toggle-window-split)
;; block-5 ends here


;; * [[file:~/config/emacs/emacs.org::*Variables][block-6]]
(setq case-fold-search nil)
(setq comint-input-ring-size 1024)
(setq default-major-mode 'org-mode)
(setq diff-switches "-u")
(setq frame-title-format "emacs:%b") ;;      (concat  "%b - emacs@" (system-name)))
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(setq minibuffer-message-timeout 0.5)
(setq nuke-trailing-whitespace-p t)
(setq parens-require-spaces nil)
(setq require-final-newline 'visit-save)
(setq tags-file-name "~/src/.tags")
(setq vc-follow-symlinks t)
(setq x-alt-keysym 'meta)
;; (visit-tags-table tags-file-name)
;; (setq font-lock-always-fontify-immediately t) where did I get that from?

(fset 'yes-or-no-p 'y-or-n-p) ;; http://www.xsteve.at/prg/emacs/.emacs.txt -- replace y-e-s by y
(put 'narrow-to-region 'disabled nil)

;; put back-up files in a single (invisible) directory in the original file's directory
(setq backup-directory-alist '(("." . ".emacs-backups")))
;; put back-up files in a single (invisible) directory in home directory -- doesn't work
;; (setq backup-directory-alist '(("~/.emacs-backups"))) 
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; (setq kill-buffer-query-functions '(lambda() t))
;; block-6 ends here


;; * [[file:~/config/emacs/emacs.org::*Browser][block-7]]
;; http://flash.metawaredesign.co.uk/2/.emacs
(if window-system
    (setq browse-url-browser-function 'browse-url-generic
          browse-url-generic-program "firefox"))
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'w3m-browse-url)
(setq browse-url-firefox-new-window-is-tab t)
;; block-7 ends here


;; * [[file:~/config/emacs/emacs.org::*Hooks][block-8]]
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

; (add-hook 'local-write-file-hooks (lambda () (nuke-trailing-whitespace))))
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; block-8 ends here


;; * [[file:~/config/emacs/emacs.org::*Elisp][block-9]]
(defun dan-emacs-lisp-mode-hook ()
  "Dan's settings for emacs-lisp mode"
  ;; (set 'lisp-indent-offset 4)
  (local-set-key "\C-c\C-l" 'dan-eval-buffer-confirm))

(add-hook 'emacs-lisp-mode-hook 'dan-emacs-lisp-mode-hook)
;; block-9 ends here


;; * [[file:~/config/emacs/emacs.org::*C%20C][block-10]]
;; Dan Feb 2006: See http://www.xemacs.org/Links/tutorials_1.html
(defun dan-c-c++-mode-hook ()
  "Dan's local settings for c-mode and c++-mode"
  ;; add font-lock to function calls (but also gets if() and while() etc)
  ;; (font-lock-add-keywords
  ;; ? ?nil `(("\\([[:alpha:]_][[:alnum:]_]*\\)(" ?1 font-lock-function-name-face)))
  (setq c-basic-offset 4)
  (setq line-number-mode t)
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	dan-c-c++-mode-keybindings))

;; (add-hook 'c-mode-hook 'c++-mode) ;; I want C++ comments, but that seems a bit heavy-handed?
(add-hook 'c-mode-hook 'dan-c-c++-mode-hook)
(add-hook 'c++-mode-hook 'dan-c-c++-mode-hook)
;; block-10 ends here


;; * [[file:~/config/emacs/emacs.org::*Python][block-11]]
(defun dan-python-mode-hook ()
  (local-set-key "\C-c\C-l" 'py-execute-buffer)
  (other-window 1)) ;; should really be change to buffer named *Python*

(add-hook 'python-mode-hook 'dan-python-mode-hook)
;; block-11 ends here


;; * [[file:~/config/emacs/emacs.org::*Functions][block-12]]
  
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

;; (defun dan-xclip-kill ()
;;   "kill region and place on X clipboard"
;;   (interactive)
;;   (shell-command-on-region (mark) (point) "xclip")
;;   (delete-region (mark) (point))) ;; don't add to kill ring

;; (defun dan-xclip-yank ()
;;   "yank from X clipboard and insert at point"
;;   (interactive)
;;   (shell-command "xclip -o" t))

(defun paste-mode ()
  (interactive)
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	dan-paste-mode-keybindings))

(defun dan-next-line-and-indent ()
  (interactive)
  (next-line)
  (indent-according-to-mode))

(defun dan-previous-line-and-indent ()
  (interactive)
  (previous-line)
  (indent-according-to-mode))

(defun dan-insert-square-brackets ()
  (interactive)
  (insert "[]")
  (backward-char))

(defun dan-insert-curly-brackets ()
  (interactive)
  (insert "{}")
  (backward-char))

(defun dan-enclose-sexp-in-parentheses ()
  (interactive)
  (insert "(")
  (forward-sexp)
  (insert ")"))

(defun dan-enclose-rest-of-line-in-parentheses ()
  (interactive)
  (insert "(")
  (end-of-line) ;; need to account for comment on same line
  (insert ")"))

(defun dan-insert-- ()
  (interactive)
  (insert "-"))

(defun dan-quote-word ()
  "Surround word at point with double quotes"
  (interactive)
  (re-search-backward "[ ,(\t]" nil t)
  (forward-char) (insert "\"")
  (re-search-forward "[ ,)\t]" nil t)
  (backward-char) (insert "\""))

(defun dan-compile-and-switch-to-iESS ()
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
;; block-12 ends here


;; * [[file:~/config/emacs/emacs.org::*Faces][block-13]]
(if nil
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:inherit nil :stipple nil :background "Grey15" :foreground "Grey" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))))
;; block-13 ends here


;; * [[file:~/config/emacs/emacs.org::*Start%20up][block-14]]
(when (string-match "^23\.*" emacs-version)
  ;; temp hack to make w3m work with emacs23
  (require 'w3m-e21)
  (provide 'w3m-e23)
  (org-agenda-list)
  (delete-other-windows))
;; block-14 ends here


;; * [[file:~/config/emacs/emacs.org::*Etc][block-15]]
;; (setq custom-file "~/src/config/emacs/emacs.el") ;; now code
;; generated by emacs' customisation buffers will go in this file rather
;; than ~/.emacs

;; Kevin Rodgers help-gnu-emacs
;; eldoc/timer can be used somehow to control how long messages appear for
;; (add-hook 'post-command-hook 'eldoc-schedule-timer nil t)
;; (add-hook 'pre-command-hook 'eldoc-pre-command-refresh-echo-area t)
;; (setq eldoc-timer [nil 1000000 0 500000 t eldoc-print-current-symbol-info nil t]) ;;
;; block-15 ends here
