(use-package denote
  :after org
  :config
  (setq denote-directory (file-truename (file-name-concat org-directory "Denotes/")))

  )
(with-eval-after-load 'org-capture
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
                 :empty-lines 1)))
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

(provide 'mayrf-emacs-denote)
