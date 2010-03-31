;;;; vanilla...

(nconc default-frame-alist '((cursor-type . bar)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)
(defun dan/sanitise-faces ()
  (interactive)
  (set-face-background 'region (face-background 'default)) ;; don't highlight region
  (set-face-background 'fringe (face-background 'default)) ;; don't have different color fringe
  (set-face-background 'highlight (face-background 'default))
  (set-face-foreground 'highlight (face-foreground 'font-lock-comment-face))
  (font-lock-fontify-buffer)
  ;; (set-face-foreground 'cursor (face-foreground 'font-lock-comment-face))
  (set-cursor-color (face-foreground 'font-lock-comment-face)))
      
(defun dan/set-show-paren-style ()
  (interactive)
  (setq show-paren-delay .125)
  (setq show-paren-style 'parenthesis)
  ;; use these in a mode hook function
  ;; (make-variable-buffer-local 'show-paren-mode)
  ;; (show-paren-mode t)
  (set-face-attribute 'show-paren-match-face nil :weight 'extra-bold)
  (set-face-background 'show-paren-match-face (face-background 'default))
  (if (boundp 'font-lock-comment-face)
      (set-face-foreground 'show-paren-match-face 
			   (face-foreground 'font-lock-comment-face))
    (set-face-foreground 'show-paren-match-face 
			 (face-foreground 'default))))
      
(add-hook 'find-file-hook 'dan/sanitise-faces)
(add-hook 'find-file-hook 'dan/set-show-paren-style)

;; this is a workaround for http://bugzilla.gnome.org/show_bug.cgi?id=514632
;; dunno about that, it did seem to work for a bit, but not really any longer
;; (setq column-number-mode t) 


(add-to-list 'load-path "/usr/local/src/emacs")
(add-to-list 'load-path "~/src/config/emacs")
(setq dan/custom-appearance t)
(if dan/custom-appearance
    (if (not window-system)
	(custom-set-faces
	 '(mode-line ((t (:foreground "red" :inverse-video nil))))
	 '(org-agenda-date-weekend ((t (:foreground "red"))) t)
	 '(org-hide ((((background light)) (:foreground "black")))))
      (progn
	(require 'color-theme)
	(unless color-theme-initialized (color-theme-initialize))
	(color-theme-charcoal-black))))
