; -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/Documents/org")
  :hook
  (org-mode . org-indent-mode)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)

  :config
  (setq org-agenda-files
        (append (seq-filter #'file-exists-p
			    (list (expand-file-name "private/todo.org" org-directory)
				  (expand-file-name "work/tasks.org" org-directory)))
		(if (file-directory-p "~/Documents/org/work/jira")
		    (directory-files "~/Documents/org/work/jira" t ".org")
		  '()
		  )
		)
	)


  (setq org-refile-files
        (seq-filter #'file-exists-p
		    (list (expand-file-name "private/someday.org" org-directory)
			  (expand-file-name "work/someday.org" org-directory))))
  (setq org-refile-targets `((nil :maxlevel . 9)
			     (org-refile-files :maxlevel . 3)))
  )

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "shared/roam" org-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (org-roam-db-autosync-mode)
  )

(use-package org-cliplink
  :after org
  :commands org-cliplink
  :bind
  (:map org-mode-map
        ("C-c l c" . org-cliplink)))

(defun my/markdown-to-org-region (start end)
  "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
  (interactive "r")
  (shell-command-on-region
   start end
   "pandoc -f markdown -t org --wrap=preserve" t t))

(use-package org-noter)

(when (getenv "JIRA_URL")
  (use-package org-jira
    :config
    (setq org-jira-working-dir (expand-file-name "work/jira" org-directory))
    (setq jiralib-url (getenv "JIRA_URL"))))

(provide 'setup-org)
