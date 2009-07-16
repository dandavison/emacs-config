;;* folding
;; [[gnus:org#87zlb6vt8m.fsf@mundaneum.com][Email from Sébastien Vauban: {Orgmode} Re: org-style foldin]]
(defun prompt-for-outline-regexp (new-regexp)
  "ask the user for a local value of outline-regexp in this buffer"
  (interactive "Outline regexp: ")
  (set (make-local-variable 'outline-regexp) new-regexp)
)

;; (global-set-key (kbd "<f9>") 'prompt-for-outline-regexp)

(defun th-outline-regexp ()
 "Calculate the outline regexp for the current mode."
 (let ((comment-starter (replace-regexp-in-string
                         "[[:space:]]+" "" comment-start)))
   (when (string= comment-start ";")
     (setq comment-starter ";;"))
   (concat "^" comment-starter "\\*+")))

(defun th-outline-minor-mode-init ()
  (interactive)
  (setq outline-regexp (th-outline-regexp))

  ;; highlight the headings
  ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
  ;; use M-x customize-apropos face to customize faces
  ;; to find the corresponding face for each outline level see org-faces.el
  (let ((heading-1-regexp (concat (substring outline-regexp 0 -1) "\\{1\\} \\(.*\\)"))
        (heading-2-regexp (concat (substring outline-regexp 0 -1) "\\{2\\} \\(.*\\)"))
        (heading-3-regexp (concat (substring outline-regexp 0 -1) "\\{3\\} \\(.*\\)"))
        (heading-4-regexp (concat (substring outline-regexp 0 -1) "\\{4,\\} \\(.*\\)"))
        )
    (font-lock-add-keywords
     nil
     `((,heading-1-regexp 1 'org-level-1 t)
       (,heading-2-regexp 1 'org-level-2 t)
       (,heading-3-regexp 1 'org-level-3 t)
       (,heading-4-regexp 1 'org-level-4 t)))))

;; (add-hook 'outline-minor-mode-hook
;;           'th-outline-minor-mode-init)


;; (org-level-1 ((t (:foreground "cornflower blue" :weight bold :height 1.8 :family "Arial"))))
;; (org-level-2 ((t (:foreground "LimeGreen" :weight bold :height 1.6 :family "Arial"))))
;; (org-level-3 ((t (:foreground "orange" :weight bold :height 1.3 :family "Arial"))))

;;* non-elisp modes
(add-hook 'outline-minor-mode-hook
  (lambda ()
    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
    (define-key outline-minor-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(add-hook 'outline-mode-hook
  (lambda ()
    (define-key outline-mode-map [(tab)] 'org-cycle)
    (define-key outline-mode-map [(backtab)] 'org-global-cycle))) ;; (shift tab) doesn't work

(defun dan-set-up-outline-minor-mode (local-outline-regexp)
  (when local-outline-regexp
    (setq outline-regexp local-outline-regexp))
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
	  (lambda () (dan-set-up-outline-minor-mode nil)))
;;		      "\\(void\\|int\\|double\\|char\\|struct\\|static\\|const\\)")))
;; (add-hook 'emacs-lisp-mode-hook 'th-outline-minor-mode-init)

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
