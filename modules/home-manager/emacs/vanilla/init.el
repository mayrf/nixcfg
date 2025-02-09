(defun my/nixos-p ()
  "Return t if operating system is NixOS, nil otherwise."
  (string-match-p "NixOS" (shell-command-to-string "uname -v")))

(defun my/nixos/get-emacs-build-date ()
  "Return NixOS Emacs build date."
  (string-match "--prefix.*emacs.*\\([[:digit:]]\\{8\\}\\)" system-configuration-options)
  (string-to-number (match-string 1 system-configuration-options)))

;; Run this before the elpaca.el is loaded. Before the installer in your init.el is a good spot.
(when (my/nixos-p) (setq elpaca-core-date (list (my/nixos/get-emacs-build-date))))
;;(setq elpaca-core-date (list (my/nixos/get-emacs-build-date)))

(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))
(setq use-package-always-ensure t)

(add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))

(use-package no-littering
  :init
  (let ((dir (no-littering-expand-var-file-name "lock-files/")))
  (make-directory dir t)
  (setq lock-file-name-transforms `((".*" ,dir t)))))

(setq backup-directory-alist '((".*" . "~/.local/share/Trash/files")))

(defun org-babel-tangle-config ()
  ;; (when (string-equal (buffer-file-name)
  ;; 		      (expand-file-name "~/.config/emacs-vanilla/mayrf-emacs.org"))
  (when (string-match "mayrf-emacs.org" (buffer-file-name))
    (let ((org-config-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'after-save-hook #'org-babel-tangle-config)))

(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

(defun my/reload-emacs ()
  (interactive)
  ;; (org-babel-tangle "~/.config/emacs-vanilla/mayrf-emacs.org")
  (my/reload-init-el)
  (my/reload-modules))

(defun my/reload-init-el ()
  (load-file "~/.config/emacs-vanilla/init.el"))


(defun my/reload-modules ()
  (interactive)
  (load-directory (locate-user-emacs-file "mayrf-emacs-modules")))
;; (mapc
;;  (lambda (string)
;;    (add-to-list 'load-path (locate-user-emacs-file string)))
;;'("prot-lisp" "prot-emacs-modules"))
;;'("mayrf-lisp" "mayrf-emacs-modules"))

  (mapc
   (lambda (string)
     (add-to-list 'load-path (locate-user-emacs-file string)))
   ;;'("prot-lisp" "prot-emacs-modules"))
   '("mayrf-lisp" "mayrf-emacs-modules"))
(require 'mayrf-emacs-keybindings)
(require 'mayrf-emacs-completion)
(require 'mayrf-emacs-style)
(require 'mayrf-emacs-org-mode)
(require 'mayrf-emacs-denote)
(require 'mayrf-emacs-magit)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
          evil-want-keybinding nil
          evil-vsplit-window-right t
          evil-split-window-below t
          evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
    (evil-mode))

(use-package evil-collection
  :after evil
  :config
  ;; Do not uncomment this unless you want to specify each and every mode
  ;; that evil-collection should works with.  The following line is here 
  ;; for documentation purposes in case you need it.  
  ;; (setq evil-collection-mode-list '(calendar dashboard dired ediff info magit ibuffer))
  (add-to-list 'evil-collection-mode-list 'help) ;; evilify help mode
  (evil-collection-init))

(use-package evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys)
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-visual-state-map "gc" 'evilnc-comment-operator))

;; Using RETURN to follow links in Org/Evil 
;; Unmap keys in 'evil-maps if not done, (setq org-return-follows-link t) will not work
;; (with-eval-after-load 'evil-maps
;;   (define-key evil-motion-state-map (kbd "SPC") nil)
;;   (define-key evil-motion-state-map (kbd "RET") nil)
;;   (define-key evil-motion-state-map (kbd "TAB") nil))
;; ;; Setting RETURN key in org-mode to follow links
;;   (setq org-return-follows-link  t)

(use-package dired-open
  :config
  (setq dired-open-extensions '(("gif" . "sxiv")
                                ("jpg" . "sxiv")
                                ("png" . "sxiv")
                                ("mkv" . "mpv")
                                ("mp4" . "mpv"))))

(use-package peep-dired
  :after dired
  :hook (evil-normalize-keymaps . peep-dired-hook)
  :config
    (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
    (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
    (evil-define-key 'normal peep-dired-mode-map (kbd "j") 'peep-dired-next-file)
    (evil-define-key 'normal peep-dired-mode-map (kbd "k") 'peep-dired-prev-file)
)

;; TODO Setup an use elfeed
(use-package elfeed
  :config
  (setq elfeed-search-feed-face ":foreground #ffffff :weight bold"
        elfeed-feeds (quote
                       (("https://www.reddit.com/r/linux.rss" reddit linux)
                        ("https://opensource.com/feed" opensource linux)))))
(use-package elfeed-goodies
  :init
  (elfeed-goodies/setup)
  :config
  (setq elfeed-goodies/entry-pane-size 0.5))





(global-visual-line-mode t)
(which-key-mode)

(global-set-key [escape] 'keyboard-escape-quit)

(use-package consult)

(use-package buffer-move)

    (recentf-mode 1)
      ;; Save what you enter into minibuffer prompts
    (setq history-length 25)
    (savehist-mode 1)
    ;; Remember and restore the last cursor location of opened files
    (save-place-mode 1)

    ;; Move customization variables to a separate file and load it
    ;; Disable the damn thing by making it disposable.
    (setq custom-file (make-temp-file "emacs-custom-"))
    ;; (setq custom-file (locate-user-emacs-file "custom-vars.el"))
    ;; (load custom-file 'noerror 'nomessage)

    ;; Don't pop up UI dialogs when prompting
    ;;(setq use-dialog-box nil)
    ;; Revert buffers when the underlying file has changed
    (global-auto-revert-mode 1)
    ;; Revert Dired and other buffers
    (setq global-auto-revert-non-file-buffers t)



  (setq custom-safe-themes t)
  (use-package ef-themes
    :config
    (load-theme 'ef-melissa-dark t nil))
  ;;(load-theme 'ef-melissa-dark)

(keymap-global-set "C-=" 'text-scale-increase)
(keymap-global-set "C--" 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

(use-package pdf-tools
  :defer t
  :commands (pdf-loader-install)
  :mode "\\.pdf\\'"
  :bind (:map pdf-view-mode-map
              ("j" . pdf-view-next-line-or-next-page)
              ("k" . pdf-view-previous-line-or-previous-page)
              ("C-=" . pdf-view-enlarge)
              ("C--" . pdf-view-shrink))
  :init (pdf-loader-install)
  :config (add-to-list 'revert-without-query ".pdf"))

(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)
                                                         (blink-cursor-mode -1)
                                                         ;; (doom-modeline-mode -1)
							 ))
