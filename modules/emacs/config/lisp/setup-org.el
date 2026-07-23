; -*- lexical-binding: t; -*-

(use-package org
  :ensure nil
  :init
  (setq org-directory "~/org")
  :hook
  (org-mode . org-indent-mode)
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)

  :config
  (setq org-agenda-files
        (append (seq-filter #'file-exists-p
			    (list (expand-file-name "todo.org" org-directory)
				  ))
		(if (file-directory-p "~/org/jira")
		    (directory-files "~/org/jira" t ".org")
		  '()
		  )
		)
	)


  (setq org-refile-files
        (seq-filter #'file-exists-p
		    (list (expand-file-name "work/someday.org" org-directory))))
  (setq org-refile-targets `((nil :maxlevel . 9)
			     (org-refile-files :maxlevel . 3)))
  )

(use-package org-roam
  :custom
  (org-roam-directory (expand-file-name "roam" org-directory))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow)
         ("C-c n j" . org-roam-dailies-capture-today))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode)

  (defun my/org-roam-dailies-timestamp-if-today ()
    "Return an HH:MM stamp, but only if the capture target date is today."
    (let ((target (or org-overriding-default-time (current-time))))
      (if (equal (format-time-string "%Y-%m-%d" target)
		 (format-time-string "%Y-%m-%d" (current-time)))
          (format-time-string "[%H:%M] ")
	"")))

  (setq org-roam-dailies-capture-templates
	`(("d" "default" entry
           "* %(my/org-roam-dailies-timestamp-if-today)%?"
           :target (file+head
                    "%<%Y-%m-%d>.org"
                    ,(format "%%[%s]"
                             (expand-file-name "templates/daily-head.org"
                                               org-roam-directory))))))
  
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


(defun my/clipboard-markdown-to-org ()
  "Convert Markdown text from clipboard and insert as Org-mode at point."
  (interactive)
  (let ((md-text (current-kill 0)))
    (if (string-empty-p md-text)
        (message "Clipboard is empty.")
      (let ((org-text
             (with-temp-buffer
               (insert md-text)
               (shell-command-on-region
                (point-min) (point-max)
                "pandoc -f markdown -t org"
                t t nil)
               (buffer-string))))
        (insert org-text)
        (message "Markdown from clipboard inserted as Org.")))))

(defun my/clipboard-org-to-markdown ()
  "Convert Org-mode text from clipboard and put Markdown back into clipboard."
  (interactive)
  (let ((org-text (current-kill 0)))
    (if (string-empty-p org-text)
        (message "Clipboard is empty.")
      (let ((md-text
             (with-temp-buffer
               (insert org-text)
               (shell-command-on-region
                (point-min) (point-max)
                "pandoc -f org -t markdown"
                t t nil)
               (buffer-string))))
        (kill-new md-text)
        (message "Org converted to Markdown and copied to clipboard.")))))

(use-package org-noter)

(when (getenv "JIRA_URL")
  (use-package org-jira
    :config
    (setq org-jira-working-dir (expand-file-name "jira" org-directory))
    (setq jiralib-url (getenv "JIRA_URL"))))

(provide 'setup-org)
