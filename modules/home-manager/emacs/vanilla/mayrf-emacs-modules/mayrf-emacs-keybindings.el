(use-package general
  :config
  (general-evil-setup)
  (general-create-definer my/leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode
  (my/leader
    "b" '(:ignore t :wk "buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-this-buffer :wk "Kill this buffer")
    "bn" '(next-buffer :wk "Next buffer")
    "bp" '(previous-buffer :wk "Previous buffer")
    "br" '(revert-buffer :wk "Reload buffer"))
  (my/leader
    "o" '(:ignore t :wk "Open")
    "oA" '(org-agenda :wk "Org Agenda"))

  (my/leader
    "f" '(:ignore t :wk "file")
    "ff" 'find-file
    "fP" '((lambda () (interactive) (find-file "~/.config/emacs-vanilla/mayrf-emacs.org")) :wk "Open Config")
    "fr" 'recentf)

  (my/leader
    "h" '(:ignore t :wk "help")
    "hrr" 'my/reload-emacs
    "hy" 'my/reload-emacs
    "hf" '(describe-function :wk "Describe function")
    "hv" '(describe-variable :wk "Describe variable")
    "hk" '(describe-variable :wk "Describe key")))

(provide 'mayrf-emacs-keybindings)
