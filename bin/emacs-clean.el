(setq package-user-dir
      (concat
       default-directory
       "elpa"))
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)
(package-refresh-contents)
