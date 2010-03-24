;; ESS
;; see http://ess.r-project.org/Manual/readme.html

(add-to-list 'load-path "/usr/local/src/emacs/ess/lisp")
(require 'ess-site)
;; (load "/usr/local/src/ess/ess/lisp/ess-site")


;; (require 'ess-eldoc)

(setq inferior-R-args "--no-save --no-restore-data --silent")
(setq safe-local-variable-values '((noweb-default-code-mode . R-mode) (outline-minor-mode)))
(autoload 'noweb-mode "noweb-mode" "Editing noweb files." t) ;; see noweb-mode.el in ESS;
(setq auto-mode-alist (append (list (cons "\\.nw$" 'noweb-mode))
			      auto-mode-alist))

(defun dan/ess-and-iess-mode-hook ()
  (setq ess-function-template " <- function() {\n\n}\n")
  (mapc (lambda (pair) (local-set-key (car pair) (cdr pair)))
	  dan/ess-and-iess-keybindings))

(defun dan/ess-mode-hook ()
  (ess-set-style 'C++))

(add-hook 'ess-mode-hook 'dan/ess-and-iess-mode-hook)
(add-hook 'inferior-ess-mode-hook 'dan/ess-and-iess-mode-hook)
(add-hook 'ess-mode-hook 'dan/ess-mode-hook)

;; See ess-help post by M. Maechler on 23 Mar 2006
(setq ess-eval-visibly-p nil)
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


;;                                 DEF GNU BSD K&R C++
;; ess-indent-level                  2   2   8   5   4
;; ess-continued-statement-offset    2   2   8   5   4
;; ess-brace-offset                  0   0  -8  -5  -4
;; ess-arg-function-offset           2   4   0   0   0
;; ess-expression-offset             4   2   8   5   4
;; ess-else-offset                   0   0   0   0   0
;; ess-close-brace-offset            0   0   0   0   0


(defun dan/ess-insert-function-template ()
  (interactive)
  (insert ess-function-template)
  (search-backward ")"))

(defun dan/ess-insert-parentheses ()
  (interactive)
  (insert-parentheses)
  (ess-r-args-show))

(defun dan/ess-kill-line-and-indent ()
  (interactive)
  (kill-line)
  (ess-indent-command))

(defun dan/ess-recover-R-process ()
  "Suppose R recover()s on an error, and you're in another window. This visits the R buffer,
exits the selector and returns"
  (interactive)
  (save-excursion 
    (ess-switch-to-end-of-ESS)
    (comint-interrupt-subjob))
  (other-window 1)) ;; seems like save-excursion doesn't do what I was hoping here

;; based on Stephen Eglen post to ess-help 21 June 2007
(defun dan/ess-list-R-function-definitions ()
  (interactive)
  (occur "<- function")
  (other-window 1))

(require 'noweb-font-lock-mode) ;; see noweb-font-lock-mode.el in ESS
;; commented above line because following during byte-compilation of emacs.el: emacs.el:258:1:Error: Cannot open load file: noweb-font-lock-mode

;; end

;; following section dates from ~2003/3 in Princeton / Chicago
;; (defun Rnw-mode ()
;;   (require 'ess-noweb)
;;   (noweb-mode)
;;   (if (fboundp 'R-mode)
;;       (setq noweb-default-code-mode 'R-mode)))
;; (add-to-list 'auto-mode-alist '("\\.Rnw\\'" . Rnw-mode))
;; (add-to-list 'auto-mode-alist '("\\.Snw\\'" . Rnw-mode))
;; (setq reftex-file-extensions
;;       '(("Snw" "Rnw" "rnw" "nw" "tex" ".tex" ".ltx") ("bib" ".bib")))
;; (setq TeX-file-extensions
;;       '("Snw" "Rnw" "rnw" "nw" "tex" "sty" "cls" "ltx" "texi" "texinfo"))
;; (setq ess-indent-level 4)
;; end


(defun RR()
  "Start an R session and evaulate some code on startup -- there must be a proper R way of doing this with .Rprofile files, but I'm not sure how to make it work regardless of what directory R is started in."
  (interactive)
  ;; (setq ess-eval-visibly-p nil) ;; doesn't stop lines being printed while evaluated - why not?
  (R)
  (ess-eval-linewise "source('~/src/common/util.R')")
  (ess-eval-linewise "options(width=140)")
  (recenter-top-bottom 0))

; (defun R-dan (&optional start-args)
;   "Start a different version of R from that in my search path"
;   (interactive "P")
;   (let ((inferior-R-program-name "/usr/src/R/R-2.5.1/bin/exec/R")) (R start-args)))

;; (custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
;; From Martin Maechler ess-help 30 June 2007
;; '(inferior-R-args "--no-save --no-restore-data --silent")
;; '(safe-local-variable-values (quote ((noweb-default-code-mode . R-mode) (outline-minor-mode)))))

;; ;; Dan Feb 2006: The following from a post by Stephen Eglen (somewhere)
;; (defun dan/inferior-ess-mode-hook ()
;;   "Dan's local settings for inferior ESS mode"
;;   (set (make-local-variable 'comint-input-ring-size) 1024) ;; saves 1024 history commands when in inferior ESS mode
;;   ;; (set (make-local-variable 'comint-input-ring-file-name) '.ess-comint-input-ring-file)
;;   (setq fill-column 140)
;;   ;; (local-set-key "\C-cr" 'ess-switch-window-exit-select-and-return)
;;   (local-set-key [C-tab] 'ess-complete-object-name)
;;   (local-set-key [C-return] 'ess-r-args-show)
;;   (local-set-key "\C-ca" 'ess-r-args-show))
;; ;;(local-set-key [C-up] 'comint-next-matching-input-from-input))

;; (defun dan/ess-mode-hook ()
;;   "Dan's local settings for ESS mode, e.g. when editing a .R file"
;;   (setq line-number-mode t)
;;   (local-set-key "\C-cr" 'ess-switch-window-exit-select-and-return)
;;   (local-set-key [C-tab] 'ess-complete-object-name)
;;   (local-set-key [C-return] 'ess-r-args-show)
;;   (local-set-key "\C-ca" 'ess-r-args-show)
;;   (local-set-key "\C-cf" 'ess-insert-function-template)
;;   (local-set-key [tab] 'comint-dynamic-complete)
;;   (local-set-key "\C-cd" 'ess-list-R-function-definitions))

;; ;; (add-hook 'inferior-ess-mode-hook 'dan-inferior-ess-mode-hook)
;; (add-hook 'ess-mode-hook 'dan/ess-mode-hook)
;; (add-hook 'ess-mode-hook (lambda () (local-set-key "\C-cd" 'ess-list-R-function-definitions)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; http://www.svenhartenstein.de/emacs-ess.php
;; (defun my-r-show-args ()
;;   "Show arguments and their default values of function in minibuffer."
;;   (interactive "*")
;;   (let ((pointposition (point)))
;;     (up-list -1)
;;     (let ((posend (point)))
;;       (backward-sexp 1)
;;       (setq object (buffer-substring-no-properties posend (point)))
;;       (ess-command (concat "try(args(" object "), silent=TRUE)\n")
;; 		   (get-buffer-create "*my-r-args-completion*"))
;;       )
;;     (goto-char pointposition)
;;     )
;;   (with-current-buffer "*my-r-args-completion*"
;;     (goto-char (point-min))
;;     (if (equal nil (search-forward "function" 10 t))
;; 	(message my-r-noargsmsg)
;;       (goto-char (point-min))
;;       (zap-to-char 1 (string-to-char "("))
;;       (goto-char (point-max))
;;       (zap-to-char -1 (string-to-char ")"))
;;       (delete-trailing-whitespace)
;;       (if (equal my-r-show-as "tooltip")
;; ;;	  (tooltip-show (concat "ARGS: " (buffer-string)))
;; 	  (tooltip-show (concat object (buffer-string)))
;; ;;	(message (concat "ARGS: " (buffer-string)))
;; 	(message (concat object "(" (buffer-string) ")"))
;; 	)))
;;   (kill-buffer "*my-r-args-completion*")
;;   )
;; (defvar my-r-noargsmsg ""
;;   "The message that is returned if my-r-show-args does not find a list
;; of arguments.")
;; (defvar my-r-show-as nil
;;   "How my-r-show-args should show the argument list. Possible values
;; are: 'message' (the default) or 'tooltip'.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
