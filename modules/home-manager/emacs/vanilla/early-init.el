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
