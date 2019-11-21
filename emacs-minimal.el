;; Conveniences / Annoyances
(setq ring-bell-function (lambda nil nil))
(fset 'yes-or-no-p 'y-or-n-p)
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; Full screen
(setq ns-use-native-fullscreen nil)
(run-with-timer 1 nil 'toggle-frame-fullscreen)

;; Server
(require 'server)
(unless (server-running-p) (server-start))

(when nil
  ;; Packages
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (require 'use-package)
  (use-package xenops :load-path "~/src/xenops"))

