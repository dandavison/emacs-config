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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(xenops yasnippet yaml-mode xterm-color wgrep wdl-mode visual-fill-column use-package undercover typescript-mode toml-mode texfrag swift-mode sqlite sql-indent smartparens simple-call-tree ripgrep restclient reformatter railscasts-theme railscasts-reloaded-theme py-isort projectile project-root project plantuml-mode paredit paradox package-build neotree modalka minimal-theme material-theme magit lsp-ui lsp-docker jsonrpc jira-markup-mode ivy-xref htmlize hindent haskell-mode graphql-mode go-mode flymake flycheck-rust flycheck-package fill-column-indicator emmet-mode elisp-lint elisp-format dot-mode dockerfile-mode docker-tramp darkroom counsel company-lean command-log-mode color-theme-modern coffee-mode cargo applescript-mode ag ace-window ace-jump-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
