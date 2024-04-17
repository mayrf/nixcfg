;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Fritz Mayr"
      user-mail-address "70516376+mayrf@users.noreply.github.com")

(setq projectile-project-search-path '("~/code" "~/repos"))

;; (setq apheleia-remote-algorithm 'local)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13 :weight 'semi-light))
(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
;; (setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-monokai-pro)
(setq doom-theme 'doom-molokai)


;; (setq doom-theme 'doom-manegarm)

(setq evil-jumps-max-length 1000)

; git reset --soft HEAD~1
(require 'magit)

(defun magit-user/magit-soft-reset-head~1 ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

  (map! :leader
        :prefix "w"
      "i" #'window-toggle-split-direction)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (defun tdr/fix-centaur-tabs ()
;; (centaur-tabs-mode -1)
;; (centaur-tabs-mode)
;; (centaur-tabs-headline-match)
;; )

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (with-selected-frame frame
;;                   (tdr/fix-centaur-tabs)))
;;               (tdr/fix-centaur-tabs))
;; )

(setq company-idle-delay 0.1)
(setq company-box-doc-delay 0.2)
(setq company-box-doc-no-wrap t)

;; (after! lsp-ui
;; (setq lsp-ui-doc-show-with-cursor t)
  ;; (setq lsp-ui-doc-enable t)
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-ui-doc-use-webkite t))
  ;; (setq lsp-ui-doc-delay 0))

(setq-default tide-user-preferences '(:importModuleSpecifierPreference "relative" :includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t))

(map! :after lsp-mode
      :leader
      :prefix "l"
      "g g" #'lsp-find-definition
      "g r" #'lsp-find-references)

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file"           "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "Z") 'dired-do-compress
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-do-kill-lines
  (kbd "% l") 'dired-downcase
  (kbd "% m") 'dired-mark-files-regexp
  (kbd "% u") 'dired-upcase
  (kbd "* %") 'dired-mark-files-regexp
  (kbd "* .") 'dired-mark-extension
  (kbd "* /") 'dired-mark-directories
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "sxiv")
                              ("jpg" . "sxiv")
                              ("png" . "sxiv")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(defun my/org-table-tab ()
  "Use `org-cycle' if inside an Org table, otherwise use original Tab functionality."
  (interactive)
  (if (org-at-table-p)
      (org-cycle)
    (if (bound-and-true-p company-mode)
        (company-indent-or-complete-common)
      (indent-for-tab-command))))

(with-eval-after-load 'org
  (with-eval-after-load 'evil
    (evil-define-key 'insert org-mode-map
      (kbd "TAB") 'my/org-table-tab)
    (evil-define-key 'insert org-mode-map
      (kbd "<tab>") 'my/org-table-tab)))

(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; (setq
;;     org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))

(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards "~/org/*.org")))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving all org-buffers except current...")
  ;; (save-some-buffers t (lambda ()
  ;;   		 (when (member (buffer-file-name) org-agenda-files)
  ;;   		   t)))
  (org-save-all-org-buffers)
  (message "Saving all org-buffers except current... done"))

;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

(setq org-reverse-note-order t)
;; Automatically get the files in "~/Documents/org"
;; with fullpath
(setq org-agenda-files
      (mapcar 'file-truename
	      (file-expand-wildcards "~/org/*.org")))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving org-agenda-files buffers...")
  (save-some-buffers t (lambda ()
			 (when (member (buffer-file-name) org-agenda-files)
			   t)))
  (message "Saving org-agenda-files buffers... done"))

;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; (setq org-roam-directory (file-truename "~/org/RoamNotes"))
(setq org-roam-directory (file-truename "~/org/roam-logseq"))
(setq org-roam-dailies-directory "journals/")
;; default roam template adds extra #+title:
(setq org-roam-capture-templates
   '(("d" "default" plain
      "%?" :target
      (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))

;; (setq org-roam-capture-templates
;;    '(("d" "default" plain
;;       "%?"
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title:${title}\n")
;;       :unnarrowed t)))

;; (use-package! websocket
;;     ;; :after org-roam)
;;     :after org)

;; (use-package! org-roam-ui
;;     ;; :after org-roam ;; or :after org
;;     :after org ;; or :after org
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BACK UP YOUR LOGSEQ DIR BEFORE RUNNING THIS!
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copyright (C) Aug 4 2022, William R. Burdick Jr.
;;
;; LICENSE
;; This code is dual-licensed with MIT and GPL licenses.
;; Take your pick and abide by whichever license appeals to you.
;;
;; logseq compatibility
;; put ids and titles at the tops of non-journal files
;; change fuzzy links from [[PAGE]] to [[id:2324234234][PAGE]]
;; also change file links to id links, provided that the links
;; expand to file names that have ids in the roam database.
;;
;; NOTE: this currently only converts fuzzy links.
;; If you have the setting :org-mode/insert-file-link? true in your Logseq config,
;; it won't convert the resulting links.
;;

;; Your logseq directory should be inside your org-roam directory,
;; put the directory you use here
(defvar bill/logseq-folder (f-expand (f-join org-roam-directory "zettel")))

;; You probably don't need to change these values
(defvar bill/logseq-pages (f-expand (f-join bill/logseq-folder "pages")))
(defvar bill/logseq-journals (f-expand (f-join bill/logseq-folder "journals")))
;;(defvar bill/rich-text-types [bold italic subscript link strike-through superscript underline inline-src-block footnote-reference inline-babel-call entity])
(defvar bill/rich-text-types '(bold italic subscript link strike-through superscript underline inline-src-block))
;; ignore files matching bill/logseq-exclude-pattern
;; example: (defvar bill/logseq-exclude-pattern (string "^" bill/logseq-folder "/bak/.*$"))
(defvar bill/logseq-exclude-pattern "^$")

(defun bill/textify (headline)
  (save-excursion
    (apply 'concat (flatten-list
                    (bill/textify-all (org-element-property :title headline))))))

(defun bill/textify-all (nodes) (mapcar 'bill/subtextify nodes))

(defun bill/with-length (str) (cons (length str) str))

(defun bill/subtextify (node)
  (cond ((not node) "")
        ((stringp node) (substring-no-properties node))
        ((member (org-element-type node) bill/rich-text-types)
         (list (bill/textify-all (cddr node))
               (if (> (org-element-property :post-blank node))
                   (make-string (org-element-property :post-blank node) ?\s)
               "")))
        (t "")))

(defun bill/logseq-journal-p (file) (string-match-p (concat "^" bill/logseq-journals) file))

(defun bill/ensure-file-id (file)
  "Visit an existing file, ensure it has an id, return whether the a new buffer was created"
  (setq file (f-expand file))
  (if (bill/logseq-journal-p file)
      `(nil . nil)
    (let* ((buf (get-file-buffer file))
           (was-modified (buffer-modified-p buf))
           (new-buf nil)
           has-data
           org
           changed
           sec-end)
      (when (not buf)
        (setq buf (find-file-noselect file))
        (setq new-buf t))
      (set-buffer buf)
      (setq org (org-element-parse-buffer))
      (setq has-data (cddr org))
      (goto-char 1)
      (when (not (and (eq 'section (org-element-type (nth 2 org))) (org-roam-id-at-point)))
        ;; this file has no file id
        (setq changed t)
        (when (eq 'headline (org-element-type (nth 2 org)))
          ;; if there's no section before the first headline, add one
          (insert "\n")
          (goto-char 1))
        (org-id-get-create)
        (setq org (org-element-parse-buffer)))
      (when (nth 3 org)
        (when (not (org-collect-keywords ["title"]))
          ;; no title -- ensure there's a blank line at the section end
          (setq changed t)
          (setq sec-end (org-element-property :end (nth 2 org)))
          (goto-char (1- sec-end))
          (when (and (not (equal "\n\n" (buffer-substring-no-properties (- sec-end 2) sec-end))))
            (insert "\n")
            (goto-char (1- (point)))
            (setq org (org-element-parse-buffer)))
          ;; copy the first headline to the title
          (insert (format "#+title: %s" (string-trim (bill/textify (nth 3 org)))))))
      ;; ensure org-roam knows about the new id and/or title
      (when changed (save-buffer))
      (cons new-buf buf))))

(defun bill/convert-logseq-file (buf)
  "convert fuzzy and file:../pages logseq links in the file to id links"
  (save-excursion
    (let* (changed
           link)
      (set-buffer buf)
      (goto-char 1)
      (while (search-forward "[[" nil t)
        (setq link (org-element-context))
        (setq newlink (bill/reformat-link link))
        (when newlink
          (setq changed t)
          (goto-char (org-element-property :begin link))
          (delete-region (org-element-property :begin link) (org-element-property :end link))
          ;; note, this format string is reall =[[%s][%s]]= but =%= is a markup char so one's hidden
          (insert newlink)))
      ;; ensure org-roam knows about the changed links
      (when changed (save-buffer)))))

(defun bill/reformat-link (link)
  (let (filename
        id
        linktext
        newlink)
    (when (eq 'link (org-element-type link))
      (when (equal "fuzzy" (org-element-property :type link))
        (setq filename (f-expand (f-join bill/logseq-pages
                                         (concat (org-element-property :path link) ".org"))))
        (setq linktext (org-element-property :raw-link link)))
      (when (equal "file" (org-element-property :type link))
        (setq filename (f-expand (org-element-property :path link)))
        (if (org-element-property :contents-begin link)
            (setq linktext (buffer-substring-no-properties
                            (org-element-property :contents-begin link)
                            (org-element-property :contents-end link)))
          (setq linktext (buffer-substring-no-properties
                          (+ (org-element-property :begin link) 2)
                          (- (org-element-property :end link) 2)))))
      (when (and filename (f-exists-p filename))
        (setq id (caar (org-roam-db-query [:select id :from nodes :where (like file $s1)]
                                          filename)))
        (when id
          (setq newlink (format "[[id:%s][%s]]%s"
                                id
                                linktext
                                (if (> (org-element-property :post-blank link))
                                    (make-string (org-element-property :post-blank link) ?\s)
                                  "")))
          (when (not (equal newlink
                            (buffer-substring-no-properties
                             (org-element-property :begin link)
                             (org-element-property :end link))))
            newlink))))))

(defun bill/roam-file-modified-p (file-path)
  (and (not (string-match-p bill/logseq-exclude-pattern file-path))
       (let ((content-hash (org-roam-db--file-hash file-path))
             (db-hash (caar (org-roam-db-query [:select hash :from files
                                                        :where (= file $s1)] file-path))))
         (not (string= content-hash db-hash)))))

(defun bill/modified-logseq-files ()
  (emacsql-with-transaction (org-roam-db)
    (seq-filter 'bill/roam-file-modified-p
                (org-roam--list-files bill/logseq-folder))))

(defun bill/check-logseq ()
  (interactive)
  (let (created
        files
        bufs
        unmodified
        cur
        bad
        buf)
    (setq files (org-roam--list-files bill/logseq-folder))
    ;; make sure all the files have file ids
    (dolist (file-path files)
      (setq file-path (f-expand file-path))
      (setq cur (bill/ensure-file-id file-path))
      (setq buf (cdr cur))
      (push buf bufs)
      (when (and (not (bill/logseq-journal-p file-path)) (not buf))
        (push file-path bad))
      (when (not (buffer-modified-p buf))
        (push buf unmodified))
      (when (car cur)
        (push buf created)))
    ;; patch fuzzy links
    (mapc 'bill/convert-logseq-file (seq-filter 'identity bufs))
    (dolist (buf unmodified)
      (when (buffer-modified-p buf)
        (save-buffer unmodified)))
    (mapc 'kill-buffer created)
    (when bad
      (message "Bad items: %s" bad))
    nil))

(setq org-agenda-custom-commands
      '(("v" "Better Agenda" (
          (agenda "")
          (tags "@computer"
                ((org-agenda-overriding-header "@computer")))
          (tags "@home"
                ((org-agenda-overriding-header "@home")))
          (tags "@work"
                ((org-agenda-overriding-header "@work")))
          (tags "@telephone"
                ((org-agenda-overriding-header "@telephone")))
          (alltodo "")))
        ("c" "@computer" (
          (tags "@computer"
                ((org-agenda-overriding-header "@computer")))))
        ("h" "@home" (
          (tags "@home"
                ((org-agenda-overriding-header "@home")))))
        ("w" "@work" (
          (tags "@work"
                ((org-agenda-overriding-header "@work")))))
        ("p" "@phone" (
          (tags "@telephone"
                ((org-agenda-overriding-header "@telephone")))))
        ))

(setq-default org-reverse-datetree-level-formats
              '("%Y"                    ; year
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
;;                "%Y W%W"                ; week
                "%Y-%m-%d %A"))           ; date

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "de_DE,en_GB,en_US,es_ES,hu_HU")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US,es_ES,hu_HU")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/cloud/machine_env/.hunspell_personal")

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))
)

(after! org
  ;; (setq org-archive-reversed-order t)
  (setq org-agenda-files '("~/org/gtd/inbox.org"
                           "~/org/gtd/inbox_phone.org"
                           "~/org/gtd/next.org"
                           "~/org/gtd/tickler.org"))

  ;; setting up inbox captures
  (setq org-capture-templates '(
               ("t" "Todo" entry
                 (file "~/org/gtd/inbox.org")
                 "* TODO %^{Brief Description} \n%?\n:LOGBOOK:\n- Added: %T\n- created from: %f\n:END:\n")

               ("r" "Rice wish" entry
                 (file+headline "~/org/gtd/next.org" "RICE")
                 "* TODO %^{Brief Description} \n%?\n:LOGBOOK:\n- Added: %T\n- created from: %f\n:END:\n")

               ("b" "book [inbox]" entry
                 (file+headline "~/org/gtd/inbox.org" "Books")
                 "* %^{author} - %^{Title}\n- recommended by %^{recommended by}\n:PROPERTIES:\n:PAGES: %^{Pages}\n:GENRE: %^{Genre}\n:LINK: %^{Link}\n:END:\n:LOGBOOK:\n - Added: %T\n- created from: %f\n:END:\n%?")

               ;; ("j" "Journal" plain
               ;;   (file+datetree "~/org/gtd/journal.org")
               ;;   "" :empty-lines-after 1)
               ("j" "Journal" plain
                    (file+function "~/org/gtd/journal.org" org-reverse-datetree-goto-date-in-file)
                    "%?" :empty-lines 1 :append nil)

               ("W" "Weekly Review" entry
                 (file+function "~/org/gtd/weekly-review.org" org-reverse-datetree-goto-date-in-file)
                 (file "~/org/gtd/templates/weekly_review.txt"))

               ("T" "Tickler" entry
                 (file+headline "~/org/gtd/tickler.org" "Tickler")
                 "* %i%? \n %U")
                                  ))
  ;; (add-to-list 'org-capture-templates
  ;;              '("t" "Todo" entry
  ;;                (file+headline "~/org/gtd/inbox.org" "TASKS")
  ;;                "* TODO %^{Brief Description} \n%?\n:LOGBOOK:\n- Added: %T\n- created from: %f\n:END:\n"))

  ;; (add-to-list 'org-capture-templates
  ;;              '("b" "book [inbox]" entry
  ;;                (file+headline "~/org/gtd/inbox.org" "Books")
  ;;                "* %^{author} - %^{Title}\n- recommended by %^{recommended by}\n:PROPERTIES:\n:PAGES: %^{Pages}\n:GENRE: %^{Genre}\n:LINK: %^{Link}\n:END:\n:LOGBOOK:\n - Added: %T\n- created from: %f\n:END:\n%?"))
  ;; (add-to-list 'org-capture-templates
  ;;              '("T" "Tickler" entry
  ;;                (file+headline "~/org/gtd/tickler.org" "Tickler")
  ;;                "* %i%? \n %U"))

  (setq org-refile-targets '(("~/org/gtd/next.org" :maxlevel . 1)
                             ("~/org/gtd/someday.org" :maxlevel . 1)
                             ("~/org/gtd/agenda.org" :maxlevel . 1)
                             ("~/org/gtd/read-review.org" :maxlevel . 1)
                             ("~/org/gtd/tickler.org" :maxlevel . 1)))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (setq org-log-done 'time))

(after! vterm
  (set-popup-rule! "*doom:vterm-popup:main" :size 0.25 :vslot -4 :select t :quit nil :ttl 0 :side 'right)
  )

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
