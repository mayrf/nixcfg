(setq use-package-always-ensure t)
(use-package evil
  :config
  (evil-mode 1)
  )

(use-package vertico
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

(use-package orderless)
