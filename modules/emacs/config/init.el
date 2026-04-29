;; -*- lexical-binding: t; -*-
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

(use-package emacs
  :ensure nil
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

(use-package evil
  :config
  (evil-mode 1)
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

(use-package consult)
(use-package prescient)

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  )


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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
