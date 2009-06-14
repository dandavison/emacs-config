(add-hook 'outline-minor-mode-hook
  (lambda ()
    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
    (define-key outline-minor-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(add-hook 'outline-mode-hook
  (lambda ()
    (define-key outline-mode-map [(tab)] 'org-cycle)
    (define-key outline-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(defun dan-set-up-outline-minor-mode (local-outline-regexp)
  (setq outline-regexp local-outline-regexp)
  ;; how does scope work in lisp? What if the function arg were named
  ;; outline-regexp?
  (outline-minor-mode t)
  (org-overview) ;; hack -- in this context, org-content only seems to
  ;; work after org-overview
  (org-content))

(defun dan-maybe-org-cycle ()
  "Cycle visibility if in a heading line; otherwise do what TAB would have done"
  (if (looking-at-p outline-regexp) (org-cycle)
    ;; else what?
))

;; where are the regexps used by font-lock kept? Should use them
;; rather than random home-grown ones.
(add-hook 'ess-mode-hook
	  (lambda () 
	    (unless (eq noweb-code-mode 'R-mode)
	      (dan-set-up-outline-minor-mode "[a-zA-Z._][a-zA-Z._0-9]* *<- *function"))))
 (add-hook 'c-mode-hook
	   (lambda () (dan-set-up-outline-minor-mode
		       "\\(void\\|int\\|double\\|char\\|struct\\|static\\|const\\)")))
(add-hook 'emacs-lisp-mode-hook 
	  (lambda () (dan-set-up-outline-minor-mode "\\((\\|;;;\\)")))
(add-hook 'python-mode-hook
	  (lambda () (dan-set-up-outline-minor-mode "\\( *def \\|if \\|class \\|##\\)")))
(add-hook 'bibtex-mode-hook
	  (lambda () (dan-set-up-outline-minor-mode "@")))

;; It's possible I should be using outline-magic
;; This is the configuration recommended outline-magic.el
;;
;; (add-hook 'outline-mode-hook 
;;           (lambda () 
;;             (require 'outline-cycle)))
            
;; (add-hook 'outline-minor-mode-hook 
;;           (lambda () 
;;             (require 'outline-magic)
;;             (define-key outline-minor-mode-map [(f10)] 'outline-cycle)))
