;; Conveniences / Annoyances
(fset 'yes-or-no-p 'y-or-n-p)
(setq
 make-backup-files nil
 create-lockfiles nil
 ring-bell-function (lambda nil nil)
 save-silently t
 vc-follow-symlinks t
 mac-command-modifier 'super
 mac-option-modifier 'meta)

;; Appearance
(blink-cursor-mode -1)
(set-face-background 'fringe (face-background 'default))
(tool-bar-mode -1)

;; Full screen
(setq ns-use-native-fullscreen nil)
(run-with-timer 1 nil 'toggle-frame-fullscreen)

;; Server
(require 'server)
(unless (server-running-p) (server-start))

;; Packages
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
