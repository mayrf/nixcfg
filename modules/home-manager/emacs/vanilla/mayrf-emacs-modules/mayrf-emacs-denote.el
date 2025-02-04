(use-package denote
  :after org
  :config
  (setq denote-directory (file-truename (file-name-concat org-directory "Denotes/")))
  )

(provide 'mayrf-emacs-denote)
