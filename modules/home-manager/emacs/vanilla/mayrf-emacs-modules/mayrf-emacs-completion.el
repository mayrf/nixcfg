(use-package vertico
  :ensure t
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 22) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; :hook (after-init . marginalia-mode))
  :config (marginalia-mode))
(provide 'mayrf-emacs-completion)

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                 ; Allows cycling through candidates
  (corfu-auto t)                  ; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert) ; insert previewed candidate
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets
  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . nil))

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)) ; Popup completion info

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0)
;;   (corfu-quit-at-boundary 'separator)
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches

;;   ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
;;   ;; be used globally (M-/).  See also the customization variable
;;   ;; `global-corfu-modes' to exclude certain modes.
;;  ;; Use TAB for cycling, default is `corfu-complete'.
;;   :bind
;;   (:map corfu-map
;;         ("M-SPC" . corfu-insert-separator)
;;         ("RET" . )
;;         ("TAB" . corfu-next)
;;         ([tab] . corfu-next)
;;         ("S-TAB" . corfu-previous)
;;         ([backtab] . corfu-previous))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode))

;; A few more useful configurations...
;; (use-package emacs
;;   :custom
;; TAB cycle if there are only few candidates
;; (completion-cycle-threshold 3)

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Emacs 30 and newer: Disable Ispell completion function.
;; Try `cape-dict' as an alternative.
(setq text-mode-ispell-word-completion nil)

;; Hide commands in M-x which do not apply to the current mode.  Corfu
;; commands are hidden, since they are not used via M-x. This setting is
;; useful beyond Corfu.
(setq read-extended-command-predicate #'command-completion-default-include-p)
;; )
