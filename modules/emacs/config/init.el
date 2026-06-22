;; -*- lexical-binding: t; -*-
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

(tool-bar-mode 0)
(menu-bar-mode 0)



(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 100)
  :custom
  (visible-bell t)
  (xref-search-program 'ripgrep)                  ;; Use a faster grep implementation for regexp search inside files 
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (global-visual-line-mode 1)                     ;; Wraps lines
  
  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.
  )

(use-package helpful
  :bind (("C-h v" . #'helpful-variable)
	 ("C-h f" . #'helpful-callable)
	 ("C-h k" . #'helpful-key)
	 ("C-h x" . #'helpful-command)
	 ("C-h F" . #'helpful-function)
         ("C-c C-d" . #'helpful-at-point))
  )
  

(use-package jinx
  :custom
  (jinx-languages "de_DE en_GB en_US es_ES hu_HU")
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;(use-package evil
;;  :config
;;  (evil-mode 1)
;;  )

(use-package avy)

(use-package vertico
  :bind
  ( :map vertico-map
    ("DEL" . vertico-directory-delete-char)) ;; Delete directories instead of chars with DEL
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  :init 
  (vertico-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult)
(use-package prescient)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  )


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; (use-package eat
;;   :config
;;   (add-hook 'eshell-load-hook #'eat-eshell-mode)
;;   )

(require 'bookmark)

;; Ensure bookmarks are loaded before checking/mutating
(bookmark-maybe-load-default-file)

(defun my-add-bookmark-if-missing (name location)
  "Add a bookmark NAME for LOCATION unless it already exists."
  (unless (bookmark-get-bookmark name t)  ; t = no-error
    (bookmark-set name nil)               ; creates bookmark at point *or* given location
    ;; overwrite the default location
    (bookmark-store name
                    `((filename . ,location))
                    nil)))

;; Example predefined bookmarks:
(my-add-bookmark-if-missing "shared" "~/Documents/org/shared/")
(my-add-bookmark-if-missing "work" "~/Documents/org/work/")
(my-add-bookmark-if-missing "init.el" "~/.config/nixcfg/modules/emacs/config/init.el")
(my-add-bookmark-if-missing "private" "~/Documents/org/private/")

;; Save modified bookmark list automatically
(bookmark-save)


;; (use-package yasnippet-snippets)
;; (use-package yasnippet 
;;   :after yasnippet-snippets
;;   :custom
;;   (yas-expand-snippet-on-input nil)
;;   :config
;;   (setq yas-snippet-dirs
;; 	`("~/.config/dotemacs/snippets"                 ;; personal snippets
;;          ,(expand-file-name "yasnippet-snippets/snippets" elpaca-repos-directory) ;; Add collection https://github.com/AndreaCrotti/yasnippet-snippets
;; 	  ;; "~/.config/dotemacs/var/elpaca/repos/yasnippet-snippets/snippets"
;;           ))

;;   (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
;;   )


(use-package embark
  :ensure t
  :commands (embark-act embark-dwim embark-bindings)
  :after evil
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
)

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package org
  :ensure nil
  :custom
  (org-directory "~/Documents/org")
  (org-agenda-files `(,(expand-file-name "private/todo.org" org-directory)))
  (org-refile-targets ())
  :bind
  ("C-c A" . org-agenda)
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

(use-package direnv
 :config
 (direnv-mode))

(use-package embark
  :bind
  (("C-." . embark-act)	 ;; pick some comfortable binding
   ("C-;" . embark-dwim) ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'(use-package embark)
  )
;; ;; UI
;; (set-fringe-mode 10)
;; (add-to-list 'custom-theme-load-path
;;              (expand-file-name "themes/" user-emacs-directory))

;; (use-package doom-themes
;;   :demand t
;;   :config
;;   (load-theme 'compline t))

;; (defun my/set-theme (variant)
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (if (string= variant "dark")
;;       (load-theme 'compline t)
;;     (load-theme 'lauds t)))

