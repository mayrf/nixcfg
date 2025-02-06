;; -*- lexical-binding: t; -*-

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Fritz Mayr"
      user-mail-address "70516376+mayrf@users.noreply.github.com")

(setq evil-jumps-max-length 1000)

(setq projectile-project-search-path '("~/code" "~/repos"))

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
;;(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13 :weight 'semi-light))
;;(setq nerd-icons-font-names '("SymbolsNerdFontMono-Regular.ttf"))
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
;; (setq doom-theme 'doom-molokai)
(setq doom-theme 'doom-henna)


;; (setq doom-theme 'doom-manegarm)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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
  (setq ispell-personal-dictionary "~/Documents/org/.hunspell_personal")

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))
)

; git reset --soft HEAD~1
(require 'magit)

(defun magit-user/magit-soft-reset-head~1 ()
  "Soft reset current git repo to HEAD~1."
  (interactive)
  (magit-reset-soft "HEAD~1"))

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

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(setq org-directory "~/Documents/org/")

;; (defun my/org-table-tab ()
;;   "Use `org-cycle' if inside an Org table, otherwise use original Tab functionality."
;;   (interactive)
;;   (if (org-at-table-p)
;;       (org-cycle)
;;     (if (bound-and-true-p company-mode)
;;         (company-indent-or-complete-common)
;;       (indent-for-tab-command))))

;; (with-eval-after-load 'org
;;   (with-eval-after-load 'evil
;;     (evil-define-key 'insert org-mode-map
;;       (kbd "TAB") 'my/org-table-tab)
;;     (evil-define-key 'insert org-mode-map
;;       (kbd "<tab>") 'my/org-table-tab)))

(setq-default org-reverse-datetree-level-formats
              '("%Y"                    ; year
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
;;                "%Y W%W"                ; week
                "%Y-%m-%d %A"))           ; date

(setq olivetti-body-width 140)
(defun org-mode-open-hook ()
  "Hook to be run when org-agenda is opened"
  (olivetti-mode))

;; Adds hook to org agenda mode, making follow mode active in org agenda
(add-hook 'org-mode-hook 'org-mode-open-hook)

;; (add-hook 'org-mode-hook #'org-modern-mode)
;; (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
;; (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
;; (setq org-modern-star nil)

;; ;; ;; Choose some fonts
;; ;; (set-face-attribute 'default nil :family "Iosevka")
;; ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
;; ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

;; ;; Add frame borders and window dividers
;; (modify-all-frames-parameters
;;  '((right-divider-width . 20)
;;    (internal-border-width . 20)))
;; (dolist (face '(window-divider
;;                 window-divider-first-pixel
;;                 window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))

;; (setq
;;  ;; Edit settings
;;  org-auto-align-tags nil
;;  org-tags-column 0
;;  org-catch-invisible-edits 'show-and-error
;;  org-special-ctrl-a/e t
;;  org-insert-heading-respect-content t

;;  ;; Org styling, hide markup etc.
;;  org-hide-emphasis-markers t
;;  org-pretty-entities t

;;  ;; Agenda styling
;;  org-agenda-tags-column 0
;;  org-agenda-block-separator ?─
;;  org-agenda-time-grid
;;  '((daily today require-timed)
;;    (800 1000 1200 1400 1600 1800 2000)
;;    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
;;  org-agenda-current-time-string
;;  "◀── now ─────────────────────────────────────────────────")

;; ;; Ellipsis styling
;; ;; (setq org-ellipsis "…")
;; ;; (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

;; ;; (global-org-modern-mode)

;; (setq
;;     org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))

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

;; Automatically get the files in "~/Documents/org"
;; with fullpath
(setq my/org-files
      (mapcar 'file-truename
	      (file-expand-wildcards (file-name-concat org-directory "*.org"))))

;; Save the corresponding buffers
(defun gtd-save-org-buffers ()
  "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
  (interactive)
  (message "Saving all org-buffers except current...")
  (save-some-buffers t (lambda ()
    		 (when (member (buffer-file-name) my/org-files)
    		   t)))
  (org-save-all-org-buffers)
  (message "Saving all org-buffers except current... done"))

;; Add it after refile
(advice-add 'org-refile :after
	    (lambda (&rest _)
	      (gtd-save-org-buffers)))
(setq org-reverse-note-order t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Logseq compatability see: https://sbgrl.me/posts/logseq-org-roam-1/
;; (setq org-directory "~/Documents/org/")
(setq org-roam-directory (file-truename (file-name-concat org-directory "roam/"))
      org-roam-dailies-directory "journals/"
      org-roam-file-exclude-regexp "\\.git/.*\\|logseq/.*$")

;; (setq org-roam-capture-project-template '("p" "project" plain
;;          ;; The file function only seems to accept literal string paths, no functions; Maybe there is an alternative "file" function, which can accept this
;;          (file "~/Documents/org/roam/Templates/ProjectTemplate.org")
;;          :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;          :unnarrowed t)
;;         ))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :target (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ;; 'org-roam-capture-project-template
        ("p" "project" plain
         ;; The file function only seems to accept literal string paths, no functions; Maybe there is an alternative "file" function, which can accept this
         (file "~/Documents/org/roam/Templates/ProjectTemplate.org")
         :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
         :unnarrowed t)
        )
      )
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org" ;; format matches Logseq
                            "#+title: %<%Y-%m-%d>\n"))))

(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun my/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

(defun my/org-roam-capture-inbox ()
  (interactive)
  (org-roam-capture- :node (org-roam-node-create)
                     :templates '(("i" "inbox" plain "* TODO %?\n/Entered on/ %U"
                                   :if-new (file+head "pages/Inbox.org" "#+title: Inbox\n")))))

(global-set-key (kbd "C-c n b") #'my/org-roam-capture-inbox)

(defun my/org-roam-project-finalize-hook ()
  "Adds the captured project file to `org-agenda-files' if the
capture was not aborted."
  ;; Remove the hook since it was added temporarily
  (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Add project file to the agenda list if the capture was confirmed
  (unless org-note-abort
    (with-current-buffer (org-capture-get :buffer)
      (add-to-list 'org-agenda-files (buffer-file-name)))))

(defun my/org-roam-find-project ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Select a project file to open, creating it if necessary
  (org-roam-node-find
   nil
   nil
   (my/org-roam-filter-by-tag "Project")
   :templates
   '(("p" "project" plain
      ;; The file function only seems to accept literal string paths, no functions; Maybe there is an alternative "file" function, which can accept this
      (file "~/Documents/org/roam/Templates/ProjectTemplate.org")
      :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
      :unnarrowed t))
   ;; '(("p" "project" plain
   ;;    (file "~/Documents/org/roam/Templates/ProjectTemplate.org")
   ;;    :if-new (file+head "pages/%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
   ;;    :unnarrowed t))
   ))

(global-set-key (kbd "C-c n p") #'my/org-roam-find-project)

(defun my/org-roam-capture-task ()
  (interactive)
  ;; Add the project file to the agenda after capture is finished
  (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

  ;; Capture the new task, creating the project file if necessary
  (org-roam-capture- :node (org-roam-node-read
                            nil
                            (my/org-roam-filter-by-tag "Project"))
                     :templates '(("p" "project" plain "** TODO %?"
                                   :if-new (file+head+olp "pages/%<%Y%m%d%H%M%S>-${slug}.org"
                                                          "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                          ("Tasks"))))))

(global-set-key (kbd "C-c n t") #'my/org-roam-capture-task)

;; (defun org-roam-dailies--capture (time &optional goto)
;;   "Capture an entry in a daily-note for TIME, creating it if necessary.

;; When GOTO is non-nil, go the note without creating an entry."
;;   (org-roam-capture- :goto (when goto '(4))
;;                      :node (org-roam-node-create)
;;                      :templates org-roam-dailies-capture-templates
;;                      :props (list :override-default-time time))
;;   (when goto (run-hooks 'org-roam-dailies-find-file-hook)))

;; (after! org-roam
;; (use-package org-roam :demand t)
(after! org
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (defun my/org-roam-copy-todo-to-today ()
    (interactive)
    (let ((org-refile-keep t) ;; Set this to nil to delete the original!
          ;; (org-roam-dailies-directory "journals/")
          (org-roam-dailies-capture-templates
           '(("t" "tasks" entry "%?"
              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
          (org-after-refile-insert-hook #'save-buffer)
          today-file
          pos)
      (save-window-excursion
        (org-roam-dailies--capture (current-time) t)

        (setq today-file (buffer-file-name))
        (message "today-file is: %s" today-file)
        (message "buffer-file-name is: %s" buffer-file-name)
        (setq pos (point)))

      ;; Only refile if the target file is different than the current file
      (unless (equal (file-truename today-file)
                     (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

  (add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (my/org-roam-copy-todo-to-today)))))

(after! org
  ;; (setq org-archive-reversed-order t)
  (setq org-agenda-files '("gtd/inbox.org"
                           "gtd/inbox_phone.org"
                           "gtd/read-review.org"
                           "gtd/someday.org"
                           "gtd/next.org"
                           "gtd/projects.org"
                           "gtd/notes.org"
                           "gtd/meeting.org"
                           "gtd/tickler.org"))

  (add-hook 'org-capture-mode-hook 'delete-other-windows)

  ;; setting up inbox captures
  (setq org-capture-templates '(
                                ("i" "inbox" entry
                                 (file "Inbox.org")
                                 "* TODO %?\n/Entered on/ %U")
                                ("m" "Meeting" entry
                                 (file+headline "gtd/tickler.org" "Future")
                                 "* %? :meeting:\n<%<%Y-%m-%d %a %H:00>>")
                                ("t" "Todo" entry
                                 (file "Inbox.org")
                                 "* TODO %^{Brief Description} \n%?\n:LOGBOOK:\n- Added: %T\n- created from: %f\n:END:\n")

                                ("r" "Rice wish" entry
                                 (file+headline "gtd/next.org" "RICE")
                                 "* TODO %^{Brief Description} \n%?\n:LOGBOOK:\n- Added: %T\n- created from: %f\n:END:\n")

                                ("b" "book [inbox]" entry
                                 (file+headline "Inbox.org" "Books")
                                 "* %^{author} - %^{Title}\n- recommended by %^{recommended by}\n:PROPERTIES:\n:PAGES: %^{Pages}\n:GENRE: %^{Genre}\n:LINK: %^{Link}\n:END:\n:LOGBOOK:\n - Added: %T\n- created from: %f\n:END:\n%?")

                                ("T" "Tickler" entry
                                 (file+headline "gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

  ;; TODO Change this to use org roam refile maybe?
  (setq org-refile-project-files (my/org-roam-list-notes-by-tag "Project"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             ;; (("roam/pages/Inbox.org"
                             ;;   "roam/pages/inbox_phone.org"
                             ;;   "roam/pages/next.org"
                             ;;   "roam/pages/read_review.org"
                             ;;   "roam/pages/someday.org"
                             ;;   "roam/pages/tickler.org") :maxlevel . 1)))
                             (("next.org"
                               "read_review.org"
                               "someday.org"
                               "tickler.org") :maxlevel . 1)
                             (org-refile-project-files :maxlevel . 1)
                             ))

  ;; Rougier org mode stuff
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     (org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))))


  (setq org-log-done 'time))

;; (package-install 'denote)
(require 'denote)

(setq denote-directory (file-truename (file-name-concat org-directory "Denotes/")))
;; (use-package! denote)

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
