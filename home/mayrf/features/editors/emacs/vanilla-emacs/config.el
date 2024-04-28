(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(setq backup-directory-alist
      `(("." . "~/.config/emacs-vanilla/backups")))

(setq auto-save-file-name-transform
      `((".*" "~/.config/emacs-vanilla/auto-save-list/" t)))

(setq custom-file (locate-user-emacs-file "custom-vars.el"))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(load-theme 'tango)
(recentf-mode 1)

(savehist-mode 1)
(setq history-length 25)
(save-place-mode 1)


;; Don't pop up UI dialof when prompting
(setq use-dialog-box nil)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)
