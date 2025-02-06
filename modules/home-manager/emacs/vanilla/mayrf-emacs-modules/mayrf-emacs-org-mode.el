(setq org-src-preserve-indentation t)
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-inbox-file (file-truename (file-name-concat org-directory "Inbox.org")))
(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)

        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@creative" . ?c)
        ("@reading" .?b)
        ("@media" .?m)
        ("@listening" .?l)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)))
(setq org-default-notes-file org-inbox-file)
(setq org-capture-templates
   '(("f" "Fleeting note" item
      (file+headline org-default-notes-file "Notes")
      "- %?")
     ("p" "Permanent note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?")))



(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

(provide 'mayrf-emacs-org-mode)

