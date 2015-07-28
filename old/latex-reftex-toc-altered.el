;; I wanted to turn off the header in reftex-toc mode
;;  (setq reftex-toc-header-face (face-background 'default)))
;; that's not right, as r-t-h-f is a face not a background colour
;; It seems to be hard-wired into this function anyway
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; A personal alteration of this function from reftex-toc.el
;;
(defun reftex-toc (&optional rebuild reuse)
  "Show the table of contents for the current document.
When called with a raw C-u prefix, rescan the document first.

Altered by Dan to not show header"

;; The REUSE argument means, search all visible frames for a window
;; displaying the toc window.  If yes, reuse this window.

  (interactive)

  (if (or (not (string= reftex-last-toc-master (reftex-TeX-master-file)))
          current-prefix-arg)
      (reftex-erase-buffer "*toc*"))

  (setq reftex-last-toc-file   (buffer-file-name))
  (setq reftex-last-toc-master (reftex-TeX-master-file))

  (set-marker reftex-toc-return-marker (point))

  ;; If follow mode is active, arrange to delay it one command
  (if reftex-toc-follow-mode
      (setq reftex-toc-follow-mode 1))

  (and reftex-toc-include-index-entries
       (reftex-ensure-index-support))
  (or reftex-support-index
      (setq reftex-toc-include-index-entries nil))

  ;; Ensure access to scanning info and rescan buffer if prefix are is '(4)
  (reftex-access-scan-info current-prefix-arg)

  (let* ((this-buf (current-buffer))
         (docstruct-symbol reftex-docstruct-symbol)
         (xr-data (assq 'xr (symbol-value reftex-docstruct-symbol)))
         (xr-alist (cons (cons "" (buffer-file-name)) (nth 1 xr-data)))
         (here-I-am (if (boundp 'reftex-rebuilding-toc)
                        (get 'reftex-toc :reftex-data)
                      (car (reftex-where-am-I))))
         (unsplittable (if (fboundp 'frame-property)
                           (frame-property (selected-frame) 'unsplittable)
                         (frame-parameter (selected-frame) 'unsplittable)))
         offset toc-window)

    (if (setq toc-window (get-buffer-window 
                          "*toc*"
                          (if reuse 'visible)))
        (select-window toc-window)
      (when (or (not reftex-toc-keep-other-windows)
                (< (window-height) (* 2 window-min-height)))
        (delete-other-windows))

      (setq reftex-last-window-width (window-width)
            reftex-last-window-height (window-height))  ; remember

      (unless unsplittable
        (if reftex-toc-split-windows-horizontally
            (split-window-horizontally
             (floor (* (window-width)
                       reftex-toc-split-windows-fraction)))
          (split-window-vertically 
           (floor (* (window-height)
                     reftex-toc-split-windows-fraction)))))

      (let ((default-major-mode 'reftex-toc-mode))
        (switch-to-buffer "*toc*")))

    (or (eq major-mode 'reftex-toc-mode) (reftex-toc-mode))
    (set (make-local-variable 'reftex-docstruct-symbol) docstruct-symbol)
    (setq reftex-toc-include-labels-indicator
          (if (eq reftex-toc-include-labels t)
              "ALL"
            reftex-toc-include-labels))
    (setq reftex-toc-include-index-indicator
          (if (eq reftex-toc-include-index-entries t)
              "ALL"
            reftex-toc-include-index-entries))

    (cond
     ((= (buffer-size) 0)
      ;; buffer is empty - fill it with the table of contents
      (message "Building *toc* buffer...")

      (setq buffer-read-only nil)
;;;       (insert (format
;;; "TABLE-OF-CONTENTS on %s
;;; SPC=view TAB=goto RET=goto+hide [q]uit [r]escan [l]abels [f]ollow [x]r [?]Help
;;; ------------------------------------------------------------------------------
;;; " (abbreviate-file-name reftex-last-toc-master)))
      
;;;       (if (reftex-use-fonts)
;;;           (put-text-property (point-min) (point) 'face reftex-toc-header-face))
;;;       (put-text-property (point-min) (point) 'intangible t)
;;;       (put-text-property (point-min) (1+ (point-min)) 'xr-alist xr-alist)

      (setq offset
            (reftex-insert-docstruct
             this-buf
             t ; include toc
             reftex-toc-include-labels
             reftex-toc-include-index-entries
             reftex-toc-include-file-boundaries
             reftex-toc-include-context
             nil ; counter
             nil ; commented
             here-I-am 
             ""     ; xr-prefix
             t      ; a toc buffer
             ))
       
      (run-hooks 'reftex-display-copied-context-hook)
      (message "Building *toc* buffer...done.")
      (setq buffer-read-only t))
     (t
      ;; Only compute the offset
      (setq offset
            (or (reftex-get-offset this-buf here-I-am
                                   (if reftex-toc-include-labels " " nil)
                                   t
                                   reftex-toc-include-index-entries
                                   reftex-toc-include-file-boundaries)
                (reftex-last-assoc-before-elt 
                 'toc here-I-am
                 (symbol-value reftex-docstruct-symbol))))
      (put 'reftex-toc :reftex-line 3)
      (goto-line 3)
      (beginning-of-line)))

    ;; Find the correct starting point
    (reftex-find-start-point (point) offset (get 'reftex-toc :reftex-line))
    (setq reftex-last-follow-point (point))
    (delete-other-windows)))
