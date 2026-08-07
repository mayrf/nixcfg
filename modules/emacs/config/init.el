; -*- lexical-binding: t; -*-
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)


(use-package no-littering
  :init
  (no-littering-theme-backups)
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
    (make-directory dir t)
    (setq lock-file-name-transforms `((".*" ,dir t))))
  )

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

(add-hook 'prog-mode-hook 'electric-pair-mode)

(require 'server)
(setq server-name "emacs")
(add-hook 'after-init-hook
          (lambda ()
            (unless (server-running-p)
              (server-start))))

(use-package emacs
  :ensure nil
  :init
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 100)
  :custom
  (visible-bell t)
  (repeat-mode t)
  (xref-search-program 'ripgrep) ;; Use a faster grep implementation for regexp search inside files 
  (column-number-mode t) ;; Display the column number in the mode line.
  (global-visual-line-mode 1) ;; Wraps lines
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.
  :config
  (recentf-mode)
  (which-key-mode)
  :hook	;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode) ;; Enable line numbers in programming modes.
  )

(use-package ef-themes
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f7>" . modus-themes-rotate)
   ("C-<f7>" . modus-themes-select)
   ("M-<f7>" . modus-themes-load-random))
  :config

  ;; Finally, load your theme of choice (or a random one with
  ;; `modus-themes-load-random', `modus-themes-load-random-dark',
  ;; `modus-themes-load-random-light').
  (modus-themes-load-theme 'ef-maris-dark))



(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("C-M-`" . popper-toggle-type))
         ("M-`"   . popper-cycle)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
	  "\\*Warnings\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package helpful
  :bind (("C-h v" . #'helpful-variable)
         ("C-h f" . #'helpful-callable)
         ("C-h k" . #'helpful-key)
         ("C-h x" . #'helpful-command)
         ("C-h F" . #'helpful-function)
         ("C-c C-d" . #'helpful-at-point))
  )

(use-package dired-preview)


(global-set-key (kbd "<f5>") #'project-recompile)
(global-set-key (kbd "S-<f5>") #'project-compile)

(defun my/compile-from-parent-dir ()
  "Run `compile' with default-directory set to the parent dir of the current file."
  (interactive)
  (let ((default-directory
         (if buffer-file-name
             (file-name-directory buffer-file-name)
           default-directory)))
    (call-interactively #'compile)))

(use-package jinx
  :custom
  (jinx-languages "de_DE en_GB en_US es_ES hu_HU")
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package avy
  :bind (("M-j" . avy-goto-char-timer)
         :map isearch-mode-map
         ("M-j" . avy-isearch))
  )

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x C-g" . magit-status))
  :config
  (defun my/magit-soft-reset-head~1 ()
    "Soft reset current git repo to HEAD~1."
    (interactive)
    (magit-reset-soft "HEAD~1"))
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  )

(use-package load-env-vars
  :config
  (load-env-vars (file-name-concat (getenv "XDG_CONFIG_HOME") "emacs/.env")))
(when (getenv "WORK_GITFORGE_HOST")
  (use-package forge
    ;; :after magit
    :config
    (setq auth-sources `(,(getenv "EMACS_AUTHINFO_PATH"))
	  work-gitforge-host (getenv "WORK_GITFORGE_HOST"))
    (add-to-list 'forge-alist `( ,work-gitforge-host ; GITHOST
			         ,(concat work-gitforge-host "/api/v4") ; APIHOST
			         ;; ,work-gitforge-host ; APIHOST
			         ,work-gitforge-host ; WEBHOST and INSTANCE-ID
			         forge-gitlab-repository) ; CLASS
	         )
    )
  )




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

(use-package consult
  :bind
  (
   ("C-x b" . consult-buffer)
   )
  )
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

(defun my-add-bookmark-if-missing (name filename)
  "Create bookmark NAME for FILENAME if it doesn't already exist."
  (unless (bookmark-get-bookmark name t)
    (bookmark-store
     name
     `((filename . ,(expand-file-name filename)))
     nil)))

;; Example predefined bookmarks:
(my-add-bookmark-if-missing "org" "~/org/")
(my-add-bookmark-if-missing "shared" "~/Documents/org/shared/")
(my-add-bookmark-if-missing "work" "~/Documents/org/work/")
(my-add-bookmark-if-missing "init.el" "~/.config/nixcfg/modules/emacs/config/init.el")
(my-add-bookmark-if-missing "private" "~/Documents/org/private/")

;; Save modified bookmark list automatically
(bookmark-save)

(use-package yasnippet
  :config
  (setq yas-snippet-dirs
	`("~/.config/dotemacs/snippets"	;; personal
	  )
	)
  )


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



(load (expand-file-name "lisp/setup-org" user-emacs-directory))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)
  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  )

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative key: M-<tab>, M-p, M-+

  :init
  (add-hook 'completion-at-point-functions #'cape-file)
)


(use-package agent-shell)

(use-package direnv
  :config
  (direnv-mode))

;; (org-excalidraw
;;  :vc (:url "https://github.com/4honor/org-excalidraw"
;; 	   :branch "main"))



(use-package embark
  :bind
  (("C-." . embark-act)	 ;; pick some comfortable binding
   ("C-;" . embark-dwim) ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'(use-package embark)
  )

(setq custom-file "~/.config/nixcfg/modules/emacs/config/custom.el")
(load custom-file)

(use-package nix-ts-mode
  :mode "\\.nix\\'")



(load (expand-file-name "lisp/kcl-ts-mode.el" user-emacs-directory))
(load (expand-file-name "lisp/org-excalidraw.el" user-emacs-directory))
(add-to-list 'treesit-language-source-alist
               '(kcl "https://github.com/kcl-lang/tree-sitter-kcl"))

;;  Install all treesitter grammars defined in treesit-install-language-grammar

(add-hook 'emacs-startup-hook
          (lambda ()
            (mapc (lambda (lang)
                    (unless (treesit-language-available-p lang)
                      (treesit-install-language-grammar lang)))
                  (mapcar #'car treesit-language-source-alist))))



(use-package ellama
  :ensure t
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-hook . ellama-chat-send-last-message)
  :init (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "qwen3.6:latest"
	   :host (or (getenv "OLLAMA_HOST") "localhost")
	   :port 80
           :embedding-model "nomic-embed-text"))
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1))

(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
(add-to-list 'major-mode-remap-alist
             '(c-or-c++-mode . c-or-c++-ts-mode))
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-to-list 'major-mode-remap-alist '(ruby-mode . ruby-ts-mode))
(add-hook 'ruby-ts-mode 'eglot-ensure)

(use-package web-mode
  :mode
  ("\\.erb\\'" . web-mode)
  )

;; (use-package just-mode
;;   :config
;;   (just-ts-mode-install-grammar))

(use-package justl)

(defun kcl-run-current-file ()
  "Run \"kcl run\" in the directory of the current buffer's file."
  (interactive)
  (let ((default-directory (file-name-directory (or (buffer-file-name)
                                                      (error "Buffer is not visiting a file")))))
    (compile "kcl run")))

(use-package pdf-tools
  :ensure nil
  :magic ("%PDF" . pdf-view-mode)
  ;; :init
  ;; (pdf-loader-install)
  ;; :config
  ;; (pdf-tools-install :no-query)
  )
