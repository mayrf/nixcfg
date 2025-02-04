(setq org-src-preserve-indentation t)
(setq org-directory "~/Documents/org/")
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

  (my/leader
    "o" '(:ignore t :wk "Open")
    "oA" '(org-agenda :wk "Org Agenda"))

(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

(provide 'mayrf-emacs-org-mode)

