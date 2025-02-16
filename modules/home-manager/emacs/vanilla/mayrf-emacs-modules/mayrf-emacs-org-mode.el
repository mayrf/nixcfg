(use-package org-download
  :custom
  (org-download-image-dir (file-name-concat org-directory "blobs/org-download"))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;; Drag-and-drop to `dired`

(use-package org-sticky-header
  :custom
  (org-sticky-header-full-path 'full)
  :config

  (add-hook 'org-mode-hook 'org-sticky-header-mode))

;; Drag-and-drop to `dired`

(defun my/gtd-file (filename)
  (file-name-concat org-directory "gtd" filename))

(setq org-reverse-note-order t)
(setq org-src-preserve-indentation t)
(setq org-directory "~/Documents/org/")

(setq my-gtd-files (mapcar
		    #'my/gtd-file
		    '("next.org"
		      "read_review.org"
		      )))

(setq org-agenda-files (append
			(directory-files-recursively
			 (file-name-concat org-directory "Denotes/projects") "\\.org$")
			my-gtd-files
			))

(setq my-refile-files (append
		       org-agenda-files
		       (mapcar
			#'my/gtd-file
			'("someday.org"
			  "inbox_phone.org"
			  "Inbox.org"
			  ))))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-inbox-file (file-truename (file-name-concat org-directory "gtd/Inbox.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
			   (my-refile-files :maxlevel . 1)))
;; (directory-files-recursively org-directory "\\.org$" :maxlevel . 1)))

;; (("next.org"
;;  "read_review.org"
;;  "someday.org"
;;  ;; (org-refile-project-files :maxlevel . 1)
;;  "tickler.org"))))
;; (directory-files-recursively org-directory "Denotes\\.org$")

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
        ("@try" .?t)
        ("@media" .?m)
        ("@listening" .?l)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)
        ("@order" . ?o)))
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
	 "* TODO %i%?")
	("K" "Cliplink capture task" entry
	 (file+headline org-default-notes-file "Links")
	 ;; "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
	 "* TODO %(org-cliplink-capture)" :empty-lines 1)
        ("N" "New note with no prompts (with denote.el)" plain
	 (file denote-last-path)
	 (function
          (lambda ()
            (denote-org-capture-with-prompts nil nil nil)))
	 :no-save t
	 :immediate-finish nil
	 :kill-buffer t
	 :jump-to-captured t)
	("j" "Journal" entry
	 (file denote-journal-extras-path-to-new-or-existing-entry)
	 "* %U %?\n%i\n%a"
	 :kill-buffer t
	 :empty-lines 1)
	("P" "New project (with Denote)" plain
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
	 :jump-to-captured t)
	))



(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        ;; (`babel-call
        ;;  (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ;; ((or `src-block `inline-src-block)
        ;;  (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

;; (general-define-key
;;  :keymaps 'org-mode-map
;;  :states 'normal
;;  "RET" '+org/dwim-at-point
;;  )

(use-package org-cliplink
  :config
  (my/leader "mlc" 'org-cliplink))
;; :after general
;; :general

(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

(provide 'mayrf-emacs-org-mode)

