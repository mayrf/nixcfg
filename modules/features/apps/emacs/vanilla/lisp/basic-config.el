(setq package-enable-at-startup nil
;;      inhibit-startup-message   t
;;     frame-resize-pixelwise    t  ; fine resize
;;      package-native-compile    t ; native compile packages
)
(scroll-bar-mode -1)               ; disable scrollbar
(tool-bar-mode -1)                 ; disable toolbar
(tooltip-mode -1)                  ; disable tooltips
;;(set-fringe-mode 10)               ; give some breathing room
(menu-bar-mode -1)                 ; disable menubar
;;(blink-cursor-mode 0)              ; disable blinking cursor
;; (setq gc-cons-threshold (* 1024 1024 1024))
;;(setq frame-inhibit-implied-resize t)
;;(setq inhibit-compacting-font-caches t)

;;(defvar file-name-handler-alist-original file-name-handler-alist)
;;(setq file-name-handler-alist nil)

:;(setq site-run-file nil)

(setq gc-cons-threshold 100000000)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
