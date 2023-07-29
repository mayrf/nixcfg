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
(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16 :weight 'semi-light))
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

(map! :after lsp-mode
      :leader
      :prefix "l"
      "g g" #'lsp-find-definition
      "g r" #'lsp-find-references)

(setq
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
)

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
(setq projectile-project-search-path '("~/code" ))

(setq org-agenda-custom-commands
      '(("v" "Better Agenda" (
          (tags "@computer"
                ((org-agenda-overriding-header "@computer")))
          (tags "@home"
                ((org-agenda-overriding-header "@home")))
          (tags "@work"
                ((org-agenda-overriding-header "@work")))
          (tags "@telephone"
                ((org-agenda-overriding-header "@telephone")))
          (agenda "")
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

               '("W" "Weekly Review" entry
                 (file+olp+datetree "~/org/gtd/weekly-review.org" )
                 (file  "~/org/gtd/templates/weekly_review.txt"))

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
