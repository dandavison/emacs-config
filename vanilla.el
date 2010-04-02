;;;; vanilla...

(nconc default-frame-alist '((cursor-type . bar)))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(blink-cursor-mode -1)
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
