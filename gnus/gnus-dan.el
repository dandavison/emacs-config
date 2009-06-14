; (add-to-list 'load-path "/usr/local/src/gnus/lisp")
; (require 'gnus)
(require 'nnmairix)
;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

(setq user-mail-address "davison@stats.ox.ac.uk")
(setq user-full-name "Dan Davison")

;;------------------------------------------------------------------------------------------
;;;
;;; Misc
;;;
;; http://people.orangeandbronze.com/~jmibanez/dotgnus.el
(require 'w3m-load)
(setq mm-text-html-renderer 'w3m)

;; http://flash.metawaredesign.co.uk/2/.gnus
(add-hook 'gnus-group-mode-hook 'color-theme-charcoal-black)

(setq gnus-read-active-file nil
      gnus-check-new-newsgroups nil)

(setq gnus-novice-user nil)
;;;
;;; Receiving and sending
;;;

(setq gnus-select-method 
      '(nnimap "dc"
	       (nnimap-address "localhost")
	       (nnimap-authinfo-file "~/config/email/authinfo")))

;; (setq gnus-select-method '(nnmaildir "email" (directory "~/Maildir/")))

;; ;; (setq gnus-secondary-select-methods '((nntp "news.gmane.org"))) ;; "news.online.no"


;; (if nil
;;     (setq mail-sources '((maildir :path "~/email-gnus/a-new" :subdirs ("cur" "new" "tmp"))
;; 			 (maildir :path "~/email-gnus/org" :subdirs ("cur" "new" "tmp")))))
(setq mail-sources 
      (mapcar 
       (lambda(dir) (list 'maildir :path dir :subdirs '("cur" "new" "tmp")))
       (directory-files "~/Maildir" nil "^[^.]")))
;; ;; (mail-source-delete-incoming t)


(setq
 send-mail-function 'sendmail-send-it ;; generates properly-formed email and sends it with
 sendmail-program "~/bin/sendmail-dan" ;; passes email over ssh to remote sendmail in Oxford
 gnus-message-archive-group "nnimap+email:a-new" ;; save outgoing mail into my default mail box
 )
;;
;;-----------------------------------------------------------------------------------------

(defun gnus-dan-summary-delete-article ()
  (interactive)
  (gnus-summary-delete-article)
  (gnus-summary-next-article))
  
(define-key gnus-summary-mode-map [delete] 'gnus-dan-summary-delete-article)


;;;
;;; Expiry
;;;
;; http://www.xemacs.org/Links/tutorials_3.html
;; turn off expiry
(remove-hook 'gnus-summary-prepare-exit-hook 'gnus-summary-expire-articles)

;; http://flash.metawaredesign.co.uk/2/.gnus
;; Don't make email expirable by default
(remove-hook 'gnus-mark-article-hook
             'gnus-summary-mark-read-and-unread-as-read)
;; (add-hook 'gnus-mark-article-hook 'gnus-summary-mark-unread-as-read) ;; don't get it

;; Only mails in these groups will expire, meaning they'll be deleted after a
;; week so long as I've read them.
(setq gnus-auto-expirable-newsgroups nil)
;; "junk\\|forums\\|gentoo-announce\\|bradsucks\\|bots\\|system\\|nnrss:.*")

;; But when I mark stuff as expireable, delete it immediately

;; (setq nnmail-expiry-wait 'immediate) ;;TMP

(setq gnus-parameters
      '((".*a-new.*"
         (expiry-wait . 'immediate))))

(defun gnus-dan-get-mail ()
  (interactive)
  ;; a hack
  (set-buffer "*Summary a-new*")
  (gnus-summary-exit)
  (set-buffer "*Group*")
  (gnus-group-get-new-news)
  (beginning-of-buffer)
  (re-search-forward "a-new")
  (gnus-group-select-group 200))


(defun gnus-group-getmail-and-get-new-news ()
  (interactive)
  (shell-command "getmail-dan")
  (gnus-group-get-new-news))

(define-key gnus-group-mode-map "g" 'gnus-group-getmail-and-get-new-news)

;;
;;------------------------------------------------------------------------------------------
;;;
;;; Summary Buffer
;;;
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
	gnus-thread-sort-by-most-recent-date))

(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-references)

(when nil
  (add-hook 'gnus-summary-prepare-hook 
	    (lambda () (end-of-buffer) (forward-line -1)))

  (add-hook 'gnus-summary-prepared-hook 
	    (lambda () (end-of-buffer) (forward-line -1)))
  )

(setq gnus-summary-line-format
      (concat
       "%0{%U%R%z%}"
       "%3{│%}" "%1{%d%}" "%3{│%}" ;; date
       "  "
       "%4{%-20,20f%}"               ;; name
       "  "
       "%3{│%}"
       " "
       "%1{%B%}"
       "%s\n"))

(setq gnus-summary-display-arrow t)

;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-sum-thread-tree-indent "  ")
  (setq gnus-sum-thread-tree-root "") ;; "● ")
  (setq gnus-sum-thread-tree-false-root "") ;; "◯ ")
  (setq gnus-sum-thread-tree-single-indent "") ;; "◎ ")
  (setq gnus-sum-thread-tree-vertical        "│")
  (setq gnus-sum-thread-tree-leaf-with-other "├─► ")
  (setq gnus-sum-thread-tree-single-leaf     "╰─► "))


;;;
;;; Article buffer
;;;
(setq gnus-visible-headers "^From:\\|^To:\\|^Cc:\\|^Subject:\\|^Date:\\|^User-Agent:\\|^X-Newsreader:")
;; Specify the order of the header lines
(setq gnus-sorted-header-list '("^From:" "^Subject:" "^User-Agent:" "^X-Newsreader:" "^Date:"))

(setq message-mode-hook (quote (orgstruct++-mode)))


;; > The gnus screenshot at http://en.wikipedia.org/wiki/Gnus
;; > is awesome! How can I get my gnus to look like that?
;; > Thanks for any tip.
;; That's my screen shot. The settings are as follows:
;; ;;; threading
;; (setq gnus-face-9 'font-lock-warning-face)
;; (setq gnus-face-10 'shadow)
;; (defun sdl-gnus-summary-line-format-ascii nil
;;   (interactive)
;;   (setq gnus-summary-line-format
;;         (concat
;;          "%0{%U%R%z%}" "%10{|%}" "%1{%d%}" "%10{|%}"
;;          "%9{%u&@;%}" "%(%-15,15f %)" "%10{|%}" "%4k" "%10{|%}"
;;          "%2u&score;" "%10{|%}" "%10{%B%}" "%s\n"))
;;   (setq
;;    gnus-sum-thread-tree-single-indent   "o "
;;    gnus-sum-thread-tree-false-root      "x "
;;    gnus-sum-thread-tree-root            "* "
;;    gnus-sum-thread-tree-vertical        "| "
;;    gnus-sum-thread-tree-leaf-with-other "|-> "
;;    gnus-sum-thread-tree-single-leaf     "+-> " ;; "\\" is _one_ char
;;    gnus-sum-thread-tree-indent          "  ")
;;   (gnus-message 5 "Using ascii tree layout."))

;; (defun sdl-gnus-summary-line-format-unicode nil
;;   (interactive)
;;   (setq gnus-summary-line-format
;;         (concat
;;          "%0{%U%R%z%}" "%10{│%}" "%1{%d%}" "%10{│%}"
;;          "%9{%u&@;%}" "%(%-15,15f %)" "%10{│%}" "%4k" "%10{│%}"
;;          "%2u&score;" "%10{│%}" "%10{%B%}" "%s\n"))
;;   (setq
;;    gnus-sum-thread-tree-single-indent   "◎ "
;;    gnus-sum-thread-tree-false-root      "  "
;;    gnus-sum-thread-tree-root            "┌ "
;;    gnus-sum-thread-tree-vertical        "│"
;;    gnus-sum-thread-tree-leaf-with-other "├─>"
;;    gnus-sum-thread-tree-single-leaf     "└─>"
;;    gnus-sum-thread-tree-indent          "  ")
;;   (gnus-message 5 "Using ascii tree layout with unicode chars."))

;; (sdl-gnus-summary-line-format-unicode)

;; -- 
;; .:  Leo  :.  [ sdl.web AT gmail.com ]  .:  [ GPG Key: 9283AA3F ]  :.


;; (setq gnus-summary-line-format "%U %3{|%} %d%10{|%} %25uz %s %3{%B%}\n")

;; seems like you can't use propertize to create colour in the summary
;; lines. I.e. the following don't work
(defun gnus-user-format-function-a (x)
  (string-match "From: \\(.*\\)" x)
  (propertize (match-string 1 x) 'face '(:foreground "blue")))
  
(defun gnus-user-format-function-z (x)
  (propertize "hello" 'face '(:foreground "red")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; http://jfm3-repl.blogspot.com/2007/12/emacs-tricks-7-use-gnus-for-email.html
;; http://www.hserus.net/wiki/index.php/Gnus


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://mah.everybody.org/docs/mail/
;;
;; (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)
;; (setq gnus-thread-hide-subtree nil)
;; (setq gnus-thread-ignore-subject t)


;; http://www.emacswiki.org/emacs/GnusFormatting
;;;     (let ((val 129))
;;;       (while (< val 160)
;;;         (aset standard-display-table val (vector (create-glyph val)))
;;;         (setq val (1+ val))))

;;;     (setq gnus-summary-line-format "%U%R%d %-5,5L %-20,20n %B%-80,80S\n"
;;;           gnus-sum-thread-tree-vertical "\232"
;;;           gnus-sum-thread-tree-root ""
;;;           gnus-sum-thread-tree-false-root ""
;;;           gnus-sum-thread-tree-indent " "
;;;           gnus-sum-thread-tree-single-indent ""
;;;           gnus-sum-thread-tree-leaf-with-other "\226\223>"
;;;           gnus-sum-thread-tree-single-leaf "\217\223>")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.xsteve.at/prg/gnus/
;; (setq gnus-summary-line-format "%O%U%R%z%d %B%(%[%4L: %-22,22f%]%) %s\n")

;; (setq gnus-summary-same-subject "")
;; (setq gnus-sum-thread-tree-root "")
;; (setq gnus-sum-thread-tree-single-indent "")
;; (setq gnus-sum-thread-tree-leaf-with-other "+-> ")
;; (setq gnus-sum-thread-tree-vertical "|")
;; (setq gnus-sum-thread-tree-single-leaf "`-> ")

;; (setq gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
