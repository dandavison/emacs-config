;;; Packages
(require 'cl)
(require 'dired-x)

(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path "~/src/3p/emacs-async")
(add-to-list 'load-path "~/src/3p/helm") (require 'helm-config)
(add-to-list 'load-path "~/src/3p/projectile") (require 'projectile)
(add-to-list 'load-path "~/src/3p/emacs-helm-ag") (require 'helm-ag)
(add-to-list 'load-path "~/src/3p/helm-projectile") (require 'helm-projectile)
(add-to-list 'load-path "~/src/3p/swiper/") (require 'ivy) (require 'counsel) (require 'swiper)
(add-to-list 'load-path "~/src/3p/magit/lisp") (require 'magit)
(add-to-list 'load-path "~/src/3p/ESS/lisp") (require 'ess)
(setq puml-plantuml-jar-path "/usr/local/Cellar/plantuml/8029/plantuml.8029.jar")
(add-to-list 'load-path "~/src/3p/puml-mode") (require 'puml-mode)

(add-to-list 'load-path "~/src/minimal") (require 'minimal)
(add-to-list 'load-path "~/src/paredit-c") (require 'paredit-c)
(add-to-list 'load-path "~/src/emacs-filter-results") (require 'filter-results)
(add-to-list 'load-path "~/src/emacs-search-files") (require 'search-files)
(add-to-list 'load-path "~/src/facet/emacs") (require 'facet)

;; (add-to-list 'custom-theme-load-path (expand-file-name
;;                                       "~/src/3p/emacs-themes-site/root/assets/local-src"))

(load-file "~/src/emacs-config/lib.el")
(when (file-exists-p "~/src/emacs-config/extra.el")
  (load-file "~/src/emacs-config/extra.el"))


;;; Modes
(add-to-list 'auto-mode-alist '("\\.compilation\\'" . compilation-mode))
(add-to-list 'auto-mode-alist '("\\.es6\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.jira\\'" . jira-markup-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;;; Server

(require 'server)
(unless (server-running-p) (server-start))


;;; Etc
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq kill-read-only-ok t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(set-default 'truncate-lines t)
(setq vc-follow-symlinks t)
(setq save-silently t)
(setq async-shell-command-buffer 'rename-buffer)

(setq-default fill-column 79)
(setq electric-indent-mode nil)
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

(set-default 'tab-width 4)

(setq auto-save-default nil)

(global-auto-revert-mode t)
(setq auto-revert-interval 0.1)
(setq global-auto-revert-non-file-buffers t)
(setq dired-auto-revert-buffer t)
(setq-default dired-omit-files-p t)
(setq dired-omit-size-limit nil)
(setq dired-omit-files "^\\.\\|__pycache__")  ;; "^\\.?#\\|^\\.$\\|^\\.\\.$" "\\.log\\|\\.aux\\|\\.out"

(dan/set-exec-path-from-shell)
(dan/set-exec-path-from-shell "PYTHONPATH")

(setq fci-rule-column 79)
(setq fci-rule-color "#A5BAF1")

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(winner-mode t)
(windmove-default-keybindings)

(setq tramp-verbose 2)

(advice-add 'goto-line :before (lambda (&rest args) (show-all)))

;;; Bookmarks
(setq bookmark-bmenu-file-column 80)
(setq bookmark-sort-flag nil)
(setq bookmark-save-flag 20)

;; message overwrites useful messages
(defun bookmark-maybe-message (fmt &rest args))

;; (advice-add 'revert-buffer :around (symbol-function 'save-excursion))


;;; Search
(setq ag-arguments
      '("--line-number" "--smart-case" "--nogroup" "--column" "--"))
(setq dan/extra-ag-arguments
      '("--follow"
        "--ignore" "*/migrations/*"
        "--ignore" "*/logs/*"
        "--ignore" "*/thirdparty/*"))


;;; Scratch buffers
(setq dan/scratch-buffer-dir "/tmp")


;;; Appearance
(setq ns-use-native-fullscreen nil)
;; Doesn't respect ns-use-native-fullscreen if called now
;; (toggle-frame-fullscreen)

(setq ring-bell-function (lambda nil nil))

(setq inhibit-startup-message t)
(setq minimal-mode-line-background "sea green")
(setq minimal-mode-line-inactive-background "dim grey")

;; (load-theme 'railscast t)
(minimal-mode)
(dan/set-appearance)

(advice-add 'load-theme :after (lambda (&rest args) (dan/set-appearance)))

;;; scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq linum-delay t)

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (sublimity-mode 1)

;;; Comint

(advice-add 'comint-previous-matching-input-from-input :after (lambda (&rest args) (goto-char (point-at-eol))))



;;; Flycheck
(setq flycheck-highlighting-mode 'lines)

;;; Org
(setq org-src-fontify-natively t)


;;; Python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(defun dan/get-default-directory-from-python-shell-maybe ()
  (let ((process (python-shell-get-process)))
    (if process
        (with-current-buffer (process-buffer process)
          default-directory)
      default-directory)))

(advice-add 'python-eldoc-function
            :around
            (lambda (orig-func &rest args)
              (let ((default-directory (dan/get-default-directory-from-python-shell-maybe)))
                (apply orig-func args))))

(advice-add 'python-shell-send-string
            :around
            (lambda (orig-func &rest args)
              (let ((default-directory (dan/get-default-directory-from-python-shell-maybe)))
                (apply orig-func args)

                ;; This could be nice to get rid of the active region after
                ;; evaluating code, but I think python-shell-send-string is
                ;; called in the background by a timer, so need to not do it then.
                ;; (deactivate-mark)
                )))

(when nil
  (advice-add 'python-shell-completion-native-get-completions
              :around
              (lambda (orig-func process import input)
                (when (or import input)
                  (apply orig-func (list process import input))))))


(require 'py-isort)
(setq py-isort-options
      '("--lines=9999999"
        "--force_single_line_imports"
        "--dont-skip=__init__.py"))

(defvar dan/python-misc-dir "~/src/misc")

;; (add-function :before (symbol-function 'run-python) 'dan/python-set-virtualenv)
;; (remove-function (symbol-function 'run-python) 'dan/python-set-virtualenv)

;;;; Python comint history
(defvar dan/python-comint-history-file "~/.ipython/history")
(defvar dan/python-comint-history-max 1000)

(defun dan/load-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  (message "Loading python comint history...")
  (mapc (lambda (item) (ring-insert+extend comint-input-ring item 'grow))
        (dan/read-comint-history file))
  (message "done"))

(defun dan/read-comint-history (file)
  (split-string (with-temp-buffer
                  (insert-file-contents file)
                  (buffer-string)) "\n" t))

(defun dan/dump-comint-history (&optional file)
  (interactive "fHistory file: ")
  (if (null comint-input-ring)
      (error "This buffer has no comint history"))
  ;; Most recent is first in comint-input-ring. Write file in
  ;; same order seeing as we are overwriting, not appending.
  (let ((history (org-uniquify (ring-elements comint-input-ring))))
    (setq history (subseq history 0 (min (length history)
                                         dan/python-comint-history-max)))
    (with-temp-buffer
      (insert (mapconcat #'identity history "\n") "\n")
      (write-file file))))


(add-hook 'kill-buffer-hook
          (lambda () (when (eq major-mode 'inferior-python-mode)
                  (dan/dump-comint-history dan/python-comint-history-file))))

(add-hook 'inferior-python-mode-hook
          (lambda () (dan/load-comint-history dan/python-comint-history-file)))

;;; Elpy
;; (add-to-list 'package-archives
;;              '("elpy" . "http://jorgenschaefer.github.io/packages/"))
;; (elpy-enable)

;;; Ido
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)


;;; Magit
(setq magit-save-repository-buffers nil)
;; (setq magit-revert-buffers nil)
(setq magit-push-always-verify nil)
(setq magit-revision-insert-related-refs nil)
(setq magit-log-arguments '("--graph" "--decorate" "-n20"))
(setq magit-status-sections-hook
      '(
        ;; magit-insert-status-headers
        magit-insert-merge-log
        magit-insert-rebase-sequence
        magit-insert-am-sequence
        magit-insert-sequencer-sequence
        magit-insert-bisect-output
        magit-insert-bisect-rest
        magit-insert-bisect-log
        ;; magit-insert-untracked-files
        magit-insert-unstaged-changes
        magit-insert-staged-changes
        ;; magit-insert-stashes
        ;; magit-insert-unpulled-commits
        ;; magit-insert-unpushed-commits
        ))

(fset 'dan/magit-diff-master
   [?\C-c ?g ?d ?r ?m ?a ?s ?t ?e ?r ?. ?. ?. return])

;;; Projectile
(setq projectile-globally-ignored-file-suffixes '("pyc" "~" "#"))
(setq projectile-buffers-filter-function 'projectile-buffers-with-file)
(setq projectile-use-git-grep t)
(add-to-list 'projectile-project-root-files-functions
             'dan/projectile-root-by-parent-directory
             'append)
(add-to-list 'projectile-project-root-files-functions
             'dan/projectile-root-default
             'append)
(add-to-list 'projectile-project-root-files-functions
             'dan/projectile-root-custom)

(add-to-list 'projectile-globally-ignored-modes "dired-mode")

(if nil
    (setq projectile-completion-system 'ivy)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))


;;; Ivy
(setq ivy-height #xFFFFFFFF)
;; (setq ivy-fixed-height-minibuffer t)

(setq counsel-git-grep-skip-counting-lines t)

(require 'popup)

(defun ivy-display-function-popup (text)
  (with-ivy-window
    (popup-tip
     (setq ivy-insert-debug
           (substring text 1))
     :nostrip t)))

;; (setq ivy-display-function 'ivy-display-function-popup)

;;; Helm
(add-to-list 'load-path "~/src/emacs-async")
(add-to-list 'load-path "~/src/helm")
(require 'helm-config)

(setq helm-input-idle-delay 0.1)
(setq helm-grep-file-path-style 'relative)
(setq helm-full-frame t)


(when nil
  ;; Results in flickering and other undesirable behavior
  (defun dan/helm-change-selection-hook-fn (&rest args)
    ;; helm-current-source seems not to be set so can't do this
    ;; (when (eq helm-current-source 'helm-themes-source))
    (condition-case nil
        (load-theme (intern (helm-get-selection)) t)
      (error nil)))
  (advice-add 'helm-next-line :after #'dan/helm-change-selection-hook-fn)
  (advice-add 'helm-previous-line :after #'dan/helm-change-selection-hook-fn))

;; (helm-add-action-to-source
;;  "`filter-results-mode'"
;;  'filter-results-helm-action
;;  helm-source-grep)

;; But helm-projectile-grep overwrites helm-source-grep, so
(setq helm-projectile-grep-or-ack-actions
      (append helm-projectile-grep-or-ack-actions
              '("`filter-results-mode'"
                filter-results-helm-action)))

(setq helm-swoop-speed-or-color 'color)
(setq helm-swoop-pre-input-function (lambda ()))
;;; Yasnippet
(setq yas/trigger-key "\C-cy")
(define-key yas/keymap [tab] 'yas/next-field-group)
(yas/initialize)
(yas/load-directory "/Users/dan/src/emacs-config/snippets")


;;; Keys

(dan/register-key-bindings
 '(global-map
   .
   (("\C-b" . backward-sexp)
    ("\C-f" . forward-sexp)
    ("\C-s" . helm-swoop)
    ([(control >)] . mc/mark-next-like-this)
    ([(super d)] . mc/mark-next-like-this)
    ([(control <)] . mc/mark-previous-like-this)
    ([(control c) (control <)] . mc/mark-all-like-this)
    ("\C-xb" . dan/switch-to-buffer)
    ("\C-x\C-f" . dan/find-file)
    ("\C-xd" . dan/dired-no-ask)
    ("\C-xp" . projectile-switch-project)
    ("\C-cb" . magit-blame)
    ("\C-ce" . show-all)
    ("\C-cf" . search-files-by-name)
    ("\C-cg" . magit-status)
    ("\C-cl" . linum-mode)
    ("\C-co" . dan/scratch-buffer)
    ("\C-cr" . replace-regexp)
    ("\C-cw" . dan/list-window-configurations)
    ("\C-c\C-l" . eval-buffer)
    ("\C-c\C-z" . python-shell-switch-to-shell)
    ("\C-c1" . flycheck-mode)
    ("\M-;" . comment-or-uncomment-region-or-line)
    ("\M-i" . dan/highlight)
    ("\M-q" . fill-paragraph)
    ("\M-x" . helm-M-x)
    ("\C-c\M-f" . search-files-thing-at-point)
    ("\C-xp" . dan/helm-projectile-switch-project)
    ("\C-z" . (lambda () (interactive)))
    ("\M-o" . dan/helm-swoop-thing-at-point)
    ([f1] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?1 arg)))
    ([f2] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?2 arg)))
    ([f3] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?3 arg)))
    ([f4] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?4 arg)))
    ([f5] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?5 arg)))
    ([f6] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?6 arg)))
    ([f7] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?7 arg)))
    ([f8] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?8 arg)))
    ([f9] . (lambda (&optional arg) (interactive "P") (dan/window-configuration ?9 arg)))
    ([f10] . dan/list-window-configurations)
    ([f11] . (lambda (&optional arg) (interactive "P") (find-file (if arg "~/src/emacs-config/lib.el" (file-chase-links "~/.emacs.d/init.el")))))
    ([f12] . facet-workon)
    ([(meta up)] . dan/transpose-line-up)
    ([(meta down)] . dan/transpose-line-down)
    ([(meta shift left)] . dan/indent-shift-left)
    ([(meta shift right)] . dan/indent-shift-right)
    ([(super ?\])] . fci-mode)
    ;; ([(super d)] . dan/bookmark-set)
    ([(super k)] . dan/bookmark-set)
    ;; ([(super k)] . (lambda (&optional arg) (interactive "P") (if arg (dan/bookmark-set) (dan/where-am-i))))
    ([(super G)] . isearch-repeat-backward)
    ([(super l)] . bookmark-bmenu-list)
    ([(super ?,)] . dan/helm-projectile-grep-no-input)
    ([(super ?.)] . dan/helm-projectile-grep-thing-at-point)
    ([(super ?\;)] . dan/show-buffer-file-name)
    ([(super ?')] . dan/iterm2-dwim)
    ([(super left)] . winner-undo)
    ([(super right)] . winner-redo)
    ([(super down)] . (lambda () (interactive) (set-mark-command t)))
    ([(shift super left)] . helm-resume)
    ([(super return)] . dan/maximize)
    ([(super |)] . dan/shell-command-on-region-and-replace)
    ([(super mouse-1)] . (lambda (event) (interactive "e") (mouse-set-point event) (dan/iterm2-dwim))))))

(global-set-key (kbd "s-,") 'dan/show-buffer-file-name)

(require 'bookmark)
(dan/register-key-bindings
 '("bookmark-bmenu" .
   (("\C-x\C-s" . bookmark-save))))

(require 'clojure-mode)
(dan/register-key-bindings
 '("clojure" .
   (("\C-x\C-e" . inf-clojure-eval-last-sexp)
    ("\C-c\C-z" . inf-clojure))))

(dan/register-key-bindings
 '("comint" .
   (([(meta up)] . comint-previous-matching-input-from-input)
    ([(meta down)] . comint-next-matching-input-from-input)
    ([(control up)] . comint-previous-matching-input-from-input)
    ([(control down)] . comint-next-matching-input-from-input)
    ("\C-l" . dan/comint-clear-buffer))))

(dan/register-key-bindings
 '("compilation" .
   (("\C-cd" . dan/delete-matching-lines))))


(dan/register-key-bindings
 '("emacs-lisp" .
   (("\C-cd" . edebug-defun)
    ("\C-c," . find-function)
    ([tab] . dan/indent-or-complete))))

(require 'helm)

(dan/register-key-bindings
 '(helm-map .
   (([(tab)] . (lambda () (interactive)))
    ([(control return)] . helm-select-action))))

(dan/register-key-bindings
 '(helm-grep-map .
   (([(left)] . backward-char)
    ([(right)] . forward-char)
    ([(control up)] . previous-history-element)
    ([(control down)] . next-history-element))))

(require 'js)
(dan/register-key-bindings
 '("js" .
   (("\C-cd" . (lambda () (interactive) (insert "debugger;"))))))


;; (require 'latex)
;; (dan/register-key-bindings
;;  '("latex" .
;;    (("$" . dan/paired-dollar)
;;     ("\C-cf" . dan/latex-frac)
;;     ("\C-c\C-c" . dan/latex-poke))))

(require 'markdown-mode)
(dan/register-key-bindings
 '("markdown" .
   (("$" . dan/paired-dollar)
    ("\M-q" . fill-paragraph)
    ;; force file write to force pelican reload
    ("\C-X\C-s" . (lambda () (interactive) (set-buffer-modified-p t) (save-buffer)))
    ([(meta left)] . left-word)
    ([(meta right)] . right-word))))


(require 'org)
(dan/register-key-bindings
 '("org" .
   (([(shift left)] . windmove-left)
    ([(shift right)] . windmove-right))))


(require 'python)
(dan/register-key-bindings
 '("python" .
   (("\C-cd" . dan/insert-ipdb-set-trace)
    ("\C-c\C-c" . dan/python-shell-send-buffer)
    (";" . self-insert-command)
    ([(super i)] . dan/python-where-am-i)
    ([(meta shift right)] . python-indent-shift-right)
    ([(meta shift left)] . python-indent-shift-left))))

(dan/register-key-bindings
 '(outline-minor-mode-map
   .
   (([(control tab)] . org-cycle)
    ([(backtab)] . org-global-cycle))))

(dan/register-key-bindings
 '("org"
   .
   (([(meta left)] . backward-word)
    ([(meta right)] . forward-word))))


;;; Mode hooks
(defun dan/on-jump-into-buffer ()
  (delete-other-windows)
  (show-all)
  (helm-highlight-current-line nil nil nil 'pulse)
  (when (eq major-mode 'python-mode)
    (dan/python-current-defun-name)))

(defun dan/after-change-major-mode-hook-fn ()
  (dan/set-appearance))
(add-hook 'after-change-major-mode-hook 'dan/after-change-major-mode-hook-fn)

(defun dan/before-save-hook-fn ()
  (dan/query-delete-trailing-whitespace)
  (when (eq major-mode 'org-mode)
    (dan/org-table-to-markdown)))
(add-hook 'before-save-hook 'dan/before-save-hook-fn)

(defun dan/c-mode-hook-fn ()
  (setq c-basic-offset 4)
  (paredit-c-mode))
(add-hook 'c-mode-hook 'dan/c-mode-hook-fn)

(defun dan/clojure-mode-hook-fn ()
  (paredit-mode)
  (inf-clojure-minor-mode))
(add-hook 'clojure-mode-hook 'dan/clojure-mode-hook-fn)
(add-hook 'clojurescript-mode-hook 'dan/clojure-mode-hook-fn)

(defun dan/inf-clojure-mode-hook-fn ()
  (paredit-mode))
(add-hook 'inf-clojure-mode-hook 'dan/inf-clojure-mode-hook-fn)

(defun dan/coffee-mode-hook-fn ()
  (paredit-c-mode)
  (setq coffee-tab-width 2))
(add-hook 'coffee-mode-hook 'dan/coffee-mode-hook-fn)

(defun dan/compilation-finish-fn ()
  (filter-results-clean-up-compilation-buffer))
(add-hook 'compilation-finish-functions 'dan/compilation-finish-fn)

(defun dan/emacs-lisp-mode-hook-fn ()
  (paredit-mode t)
  (setq prettify-symbols-alist '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "\\((\\|;;;\\)"))
(add-hook 'emacs-lisp-mode-hook 'dan/emacs-lisp-mode-hook-fn)

(defun dan/eshell-mode-hook-fn ()
  (paredit-mode t)
  (dan/pretty-lambdas))
(add-hook 'eshell-mode-hook 'dan/eshell-mode-hook-fn)

(defun dan/r-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'ess-mode-hook 'dan/r-mode-hook-fn)
(add-hook 'inferior-ess-mode-hook 'dan/r-mode-hook-fn)

(defun dan/find-function-after-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'find-function-after-hook 'dan/find-function-after-hook-fn)

(defun dan/html-mode-hook-fn ()
  (zencoding-mode))
(add-hook 'html-mode-hook 'dan/html-mode-hook-fn)

(defun dan/inferior-python-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'inferior-python-mode-hook 'dan/inferior-python-mode-hook-fn)

(defun dan/js-mode-hook-fn ()
  (paredit-c-mode)
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'dan/js-mode-hook-fn)

(defun dan/latex-mode-hook-fn ()
  (interactive)
  (dan/setup-paired-characters)
  (local-set-key "\C-c\C-c" 'dan/latex-poke))
(add-hook 'latex-mode-hook 'dan/latex-mode-hook-fn)

(defun dan/magit-diff-mode-hook-fn ()
  (dan/magit-hide-all-sections))
(add-hook 'magit-diff-mode-hook 'dan/magit-diff-mode-hook-fn)

(defun dan/makefile-mode-hook-fn ()
  (paredit-c-mode))
(add-hook 'makefile-mode-hook 'dan/makefile-mode-hook-fn)

(defun dan/minibuffer-setup-hook-fn ()
  (when (eq this-command 'eval-expression)
    (setq completion-at-point-functions '(lisp-completion-at-point t))
    (local-set-key [tab] 'complete-symbol)
    (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'dan/minibuffer-setup-hook-fn)

(defun dan/next-error-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'next-error-hook 'dan/next-error-hook-fn)

(defun dan/occur-mode-find-occurrence-hook-fn ()
  (dan/on-jump-into-buffer))
(add-hook 'occur-mode-find-occurrence-hook 'dan/occur-mode-find-occurrence-hook-fn)

(add-hook 'helm-goto-line-before-hook 'dan/on-jump-into-buffer)

(defun dan/paredit-c-mode-hook-fn ()
  (local-set-key [(meta up)] 'dan/transpose-line-up)
  (local-set-key [(meta down)] 'dan/transpose-line-down)
  (local-set-key ";" 'self-insert-command))
(add-hook 'paredit-c-mode-hook 'dan/paredit-c-mode-hook-fn)

(defun dan/python-mode-hook-fn ()
  (paredit-c-mode)
  (setq prettify-symbols-alist
        '(("lambda" . 955)))
  (prettify-symbols-mode)
  (dan/set-up-outline-minor-mode "[ \t]*\\(def .+\\|class .+\\|##\\)"))
(add-hook 'python-mode-hook 'dan/python-mode-hook-fn)
(put 'dired-find-alternate-file 'disabled nil)

(defun dan/scheme-mode-hook-fn ()
  (scheme-mode))
(add-hook 'scheme-mode-hook 'dan/scheme-mode-hook-fn)

(defun dan/sh-mode-hook-fn ()
  (setq sh-indentation 4)
  (setq sh-basic-offset nil)
  (paredit-c-mode))
(add-hook 'sh-mode-hook 'dan/sh-mode-hook-fn)


;;; Spam

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("9e6ac467fa1e5eb09e2ac477f61c56b2e172815b4a6a43cf48def62f9d3e5bf9" "b9183de9666c3a16a7ffa7faaa8e9941b8d0ab50f9aaba1ca49f2f3aec7e3be9" "0e8c264f24f11501d3f0cabcd05e5f9811213f07149e4904ed751ffdcdc44739" "780c67d3b58b524aa485a146ad9e837051918b722fd32fd1b7e50ec36d413e70" "a11043406c7c4233bfd66498e83600f4109c83420714a2bd0cd131f81cbbacea" "45482e7ddf47ab1f30fe05f75e5f2d2118635f5797687e88571842ff6f18b4d5" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3b4800ea72984641068f45e8d1911405b910f1406b83650cbd747a831295c911" default)))
 '(magit-diff-arguments (quote ("--ignore-all-space" "--no-ext-diff")))
 '(package-selected-packages
   (quote
    (multiple-cursors ivy counsel use-package sublimity avy auctex-latexmk smooth-scroll soothe-theme debbugs fzf helm-swoop elpy transpose-frame helm-themes graphviz-dot-mode helm-projectile flycheck color-theme-modern zones py-isort jira-markup-mode inf-clojure auto-overlays aumix-mode buffer-move confluence ess zencoding-mode yasnippet-bundle yasnippet yaml-mode smartparens rust-mode railscasts-theme paredit-everywhere minimal-theme markdown-mode latex-pretty-symbols flx-ido fill-column-indicator eyuml evil dockerfile-mode dired-details+ color-theme-railscasts coffee-mode clojure-mode auctex ag)))
 '(safe-local-variable-values (quote ((bug-reference-bug-regexp . "#\\(?2:[0-9]+\\)")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(message "⚡")
