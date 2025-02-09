(use-package denote
  :after org
  :config
  (setq denote-directory (file-truename (file-name-concat org-directory "Denotes/")))

  )
;; (with-eval-after-load 'org-capture
(add-to-list 'org-capture-templates
             '("N" "New note with no prompts (with denote.el)" plain
	       (file denote-last-path)
	       (function
                (lambda ()
                  (denote-org-capture-with-prompts nil nil nil)))
	       :no-save t
	       :immediate-finish nil
	       :kill-buffer t
	       :jump-to-captured t))
(add-to-list 'org-capture-templates
             '("j" "Journal" entry
               (file denote-journal-extras-path-to-new-or-existing-entry)
               "* %U %?\n%i\n%a"
               :kill-buffer t
               :empty-lines 1))
;; TODO Add hook to automatically add the new file to agenda, until then, just reload config
(add-to-list 'org-capture-templates
	     '("P" "New project (with Denote)" plain
	       (file denote-last-path)
	       (function
		(lambda ()
                  (let ((denote-use-directory (expand-file-name "projects" (denote-directory)))
			;; TODO Enable adding of additional keywords
			(denote-use-keywords '("project"))
			(denote-org-capture-specifiers (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
			(denote-prompts (denote-add-prompts '(keywords)))

			(denote-org-front-matter
			 (concat "#+title:      %s\n"
				 "#+date:       %s\n"
				 "#+filetags:   %s\n"
				 "#+identifier: %s\n"
				 "#+category: %1$s\n"
				 "\n")
			 ))
		    (denote-org-capture))))
	       :no-save t
	       :immediate-finish nil
	       :kill-buffer t
	       :jump-to-captured t))
;; )
(defun my-denote-region-org-structure-template (_beg _end)
  (when (derived-mode-p 'org-mode)
    (activate-mark)
    (call-interactively 'org-insert-structure-template)))

;; TODO Maybe also add a link to the source?
(add-hook 'denote-region-after-new-note-functions #'my-denote-region-org-structure-template)


;; Variant of `my-denote-region' to reference the source

(defun my-denote-region-get-source-reference ()
  "Get a reference to the source for use with `my-denote-region'.
The reference is a URL or an Org-formatted link to a file."
  ;; We use a `cond' here because we can extend it to cover move
  ;; cases.
  (cond
   ((derived-mode-p 'eww-mode)
    (plist-get eww-data :url))
   ;; Here we are just assuming an Org format.  We can make this more
   ;; involved, if needed.
   (buffer-file-name
    (format "[[file:%s][%s]]" buffer-file-name (buffer-name)))))

(defun my-denote-region ()
  "Like `denote-region', but add the context afterwards.
For how the context is retrieved, see `my-denote-region-get-source-reference'."
  (interactive)
  (let ((context (my-denote-region-get-source-reference)))
    (call-interactively 'denote-region)
    (when context
      (goto-char (point-max))
      (insert "\n")
      (insert context))))

;; Add quotes around snippets of text captured with `denote-region' or `my-denote-region'.

(defun my-denote-region-org-structure-template (beg end)
  "Automatically quote (with Org syntax) the contents of `denote-region'."
  (when (derived-mode-p 'org-mode)
    (goto-char end)
    (insert "#+end_quote\n")
    (goto-char beg)
    (insert "#+begin_quote\n")))

(add-hook 'denote-region-after-new-note-functions #'my-denote-region-org-structure-template)

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; (defun my-denote-template (template-file-name)
;;   (lambda ()
;;     (file-to-string (file-name-concat user-emacs-directory "templates" template-file-name))))
;; ;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(defun my-weekly-review-template ()
  ;; (interactive)
  (file-to-string "~/Documents/org/gtd/templates/weekly_review.org"))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(defun my-daily-journal-template ()
  ;; (interactive)
  (file-to-string "templates/daily_journal.org"))

(defun my-project-template ()
  ;; (interactive)
  (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(setq denote-templates '((weekly_review . my-weekly-review-template)
			 (daily_journal . my-daily-journal-template)
			 (project . my-project-template)
			 ;; (theproject . (my-denote-template "project.org"))
			 ))

;; (message (file-to-string "~/Documents/org/gtd/templates/weekly_review.txt"))

(provide 'mayrf-emacs-denote)
