;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Fritz Mayr"
      user-mail-address "70516376+mayrf@users.noreply.github.com")

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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 14 :weight 'semi-light))
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
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-manegarm)

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

(setq
 org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿"))


;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-roam-directory (file-truename "~/org/RoamNotes"))
;; default roam template adds extra #+title:
;; (setq org-roam-capture-templates
;;    '(("d" "default" plain
;;       "%?"
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title:${title}\n")
;;       :unnarrowed t)))
(setq projectile-project-search-path '("~/code"))

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
                             ((org-agenda-overriding-header "@telephone")))))))


(setq-default org-reverse-datetree-level-formats
              '("%Y"                    ; year
                (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-monday time))) ; month
                "%Y W%W"                ; week
                "%Y-%m-%d %A"))           ; date


(setq org-caldav-url "https://yemenroad.duckdns.org/remote.php/dav/calendars/Ostpol")

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

                                ("b" "book [inbox]" entry
                                 (file+headline "~/org/gtd/inbox.org" "Books")
                                 "* %^{author} - %^{Title}\n- recommended by %^{recommended by}\n:PROPERTIES:\n:PAGES: %^{Pages}\n:GENRE: %^{Genre}\n:LINK: %^{Link}\n:END:\n:LOGBOOK:\n - Added: %T\n- created from: %f\n:END:\n%?")

                                ;; ("j" "Journal" plain
                                ;;   (file+datetree "~/org/gtd/journal.org")
                                ;;   "" :empty-lines-after 1)
                                ("z" "Journal test" plain
                                 (file+function "~/org/gtd/journal.org" org-reverse-datetree-goto-date-in-file)
                                 "%?" :empty-lines 1 :append nil)

                                ("W" "Weekly Review" entry
                                 (file+olp+datetree "~/org/gtd/weekly-review.org")
                                 (file "~/org/gtd/templates/weekly_review.txt"))

                                ("T" "Tickler" entry
                                 (file+headline "~/org/gtd/tickler.org" "Tickler")
                                 "* %i%? \n %U")))

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
