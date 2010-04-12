;;;; vanilla...

(nconc default-frame-alist '((cursor-type . bar)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)
(add-to-list 'load-path "/usr/local/src/emacs")
(add-to-list 'load-path "~/src/config/emacs")
(setq dan/custom-appearance nil)
(if dan/custom-appearance
    (if (not window-system)
	(custom-set-faces
	 '(mode-line ((t (:foreground "red" :inverse-video nil))))
	 '(org-agenda-date-weekend ((t (:foreground "red"))) t)
	 '(org-hide ((((background light)) (:foreground "black")))))
      (progn
	(add-to-list 'load-path "/usr/local/src/emacs/color-theme-6.6.0")
	(require 'color-theme)
	(eval-after-load "color-theme"
	  '(progn
	     (color-theme-initialize)
	     (color-theme-charcoal-black))))))
