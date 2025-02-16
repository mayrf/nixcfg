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
  (my/reload-init-el))
  ;; (my/reload-modules))

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

;;   (mapc
;;    (lambda (string)
;;      (add-to-list 'load-path (locate-user-emacs-file string)))
;;    ;;'("prot-lisp" "prot-emacs-modules"))
;;    '("mayrf-lisp" "mayrf-emacs-modules"))
;; (require 'mayrf-emacs-keybindings)
;; (require 'mayrf-emacs-completion)
;; (require 'mayrf-emacs-style)
;; (require 'mayrf-emacs-org-mode)
;; (require 'mayrf-emacs-denote)
;; (require 'mayrf-emacs-magit)

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

(define-key minibuffer-local-map (kbd "C-v") 'yank)

(set-face-attribute 'default nil :height 110)

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

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package general
  :config
  (general-evil-setup)

  ;; set up 'SPC' as the global leader key
  (general-create-definer my/leader
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC" ;; set leader
    :global-prefix "M-SPC") ;; access leader in insert mode

  (my/leader
    ;; "SPC" '(counsel-M-x :wk "Counsel M-x")
    "." '(find-file :wk "Find file"))
  ;; "=" '(perspective-map :wk "Perspective") ;; Lists all the perspective keybindings
  ;; "TAB TAB" '(comment-line :wk "Comment lines")
  ;; "u" '(universal-argument :wk "Universal argument"))

  ;; (dt/leader-keys
  ;;  "a" '(:ignore t :wk "A.I.")
  ;;  "a a" '(ellama-ask-about :wk "Ask ellama about region")
  ;;  "a e" '(:ignore t :wk "Ellama enhance")
  ;;  "a e g" '(ellama-improve-grammar :wk "Ellama enhance wording")
  ;;  "a e w" '(ellama-improve-wording :wk "Ellama enhance grammar")
  ;;  "a i" '(ellama-chat :wk "Ask ellama")
  ;;  "a p" '(ellama-provider-select :wk "Ellama provider select")
  ;;  "a s" '(ellama-summarize :wk "Ellama summarize region")
  ;;  "a t" '(ellama-translate :wk "Ellama translate region"))
  
  (my/leader
    "b" '(:ignore t :wk "Bookmarks/Buffers")
    "b b" '(switch-to-buffer :wk "Switch to buffer")
    "b c" '(clone-indirect-buffer :wk "Create indirect buffer copy in a split")
    "b C" '(clone-indirect-buffer-other-window :wk "Clone indirect buffer in new window")
    "b d" '(bookmark-delete :wk "Delete bookmark")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-current-buffer :wk "Kill current buffer")
    "b K" '(kill-some-buffers :wk "Kill multiple buffers")
    "b l" '(list-bookmarks :wk "List bookmarks")
    "b m" '(bookmark-set :wk "Set bookmark")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b R" '(rename-buffer :wk "Rename buffer")
    "b s" '(basic-save-buffer :wk "Save buffer")
    "b S" '(save-some-buffers :wk "Save multiple buffers")
    "b w" '(bookmark-save :wk "Save current bookmarks to bookmark file"))

  (my/leader
    "d" '(:ignore t :wk "Dired")
    "d d" '(dired :wk "Open dired")
    "d f" '(wdired-finish-edit :wk "Writable dired finish edit")
    "d j" '(dired-jump :wk "Dired jump to current")
    "d n" '(neotree-dir :wk "Open directory in neotree")
    "d p" '(peep-dired :wk "Peep-dired")
    "d w" '(wdired-change-to-wdired-mode :wk "Writable dired"))

  (my/leader
    "e" '(:ignore t :wk "Ediff/Eshell/Eval/EWW")    
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e d" '(eval-defun :wk "Evaluate defun containing or after point")
    "e e" '(eval-expression :wk "Evaluate and elisp expression")
    "e f" '(ediff-files :wk "Run ediff on a pair of files")
    "e F" '(ediff-files3 :wk "Run ediff on three files")
    "e h" '(counsel-esh-history :which-key "Eshell history")
    "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
    "e n" '(eshell-new :wk "Create new eshell buffer")
    "e r" '(eval-region :wk "Evaluate elisp in region")
    "e R" '(eww-reload :which-key "Reload current page in EWW")
    "e s" '(eshell :which-key "Eshell")
    "e w" '(eww :which-key "EWW emacs web wowser"))

  (my/leader
    "f" '(:ignore t :wk "Files")    
    "SPC" '(project-find-file :wk "Find File in Project")
    "f P" '((lambda () (interactive) (find-file "~/.config/emacs-vanilla/mayrf-emacs.org")) :wk "Open Config")
    ;; "f c" '((lambda () (interactive)
    ;;           (find-file "~/.config/emacs/config.org")) 
    ;;         :wk "Open emacs config.org")
    ;; "f e" '((lambda () (interactive)
    ;;           (dired "~/.config/emacs/")) 
    ;;         :wk "Open user-emacs-directory in dired")
    "f d" '(find-grep-dired :wk "Search for string in files in DIR")
    "f g" '(counsel-grep-or-swiper :wk "Search for string current file")
    ;; "f i" '((lambda () (interactive)
    ;;           (find-file "~/.config/emacs/init.el")) 
    ;;         :wk "Open emacs init.el")

    "f f" 'find-file
    ;; "f f" '(consult-find :wk "Find a file")
    ;; "f r" 'recentf)
    ;; "f j" '(counsel-file-jump :wk "Jump to a file below current directory")
    "f l" '(consult-locate :wk "Locate a file")
    "f r" '(consult-recent-file :wk "Find recent files"))
  ;; "f u" '(sudo-edit-find-file :wk "Sudo find file")
  ;; "f U" '(sudo-edit :wk "Sudo edit file"))

  (my/leader
    "g" '(:ignore t :wk "Git")    
    "g /" '(magit-displatch :wk "Magit dispatch")
    "g ." '(magit-file-displatch :wk "Magit file dispatch")
    "g b" '(magit-branch-checkout :wk "Switch branch")
    "g c" '(:ignore t :wk "Create") 
    "g c b" '(magit-branch-and-checkout :wk "Create branch and checkout")
    "g c c" '(magit-commit-create :wk "Create commit")
    "g c f" '(magit-commit-fixup :wk "Create fixup commit")
    "g C" '(magit-clone :wk "Clone repo")
    "g f" '(:ignore t :wk "Find") 
    "g f c" '(magit-show-commit :wk "Show commit")
    "g f f" '(magit-find-file :wk "Magit find file")
    "g f g" '(magit-find-git-config-file :wk "Find gitconfig file")
    "g F" '(magit-fetch :wk "Git fetch")
    "g g" '(magit-status :wk "Magit status")
    "g i" '(magit-init :wk "Initialize git repo")
    "g l" '(magit-log-buffer-file :wk "Magit buffer log")
    "g r" '(vc-revert :wk "Git revert file")
    "g s" '(magit-stage-file :wk "Git stage file")
    "g t" '(git-timemachine :wk "Git time machine")
    "g u" '(magit-stage-file :wk "Git unstage file"))


  (my/leader
    "h" '(:ignore t :wk "Help")
    ;; "h a" '(counsel-apropos :wk "Apropos")
    "h b" '(describe-bindings :wk "Describe bindings")
    "h c" '(describe-char :wk "Describe character under cursor")
    "h d" '(:ignore t :wk "Emacs documentation")
    "h d a" '(about-emacs :wk "About Emacs")
    "h d d" '(view-emacs-debugging :wk "View Emacs debugging")
    "h d f" '(view-emacs-FAQ :wk "View Emacs FAQ")
    "h d m" '(info-emacs-manual :wk "The Emacs manual")
    "h d n" '(view-emacs-news :wk "View Emacs news")
    "h d o" '(describe-distribution :wk "How to obtain Emacs")
    "h d p" '(view-emacs-problems :wk "View Emacs problems")
    "h d t" '(view-emacs-todo :wk "View Emacs todo")
    "h d w" '(describe-no-warranty :wk "Describe no warranty")
    "h e" '(view-echo-area-messages :wk "View echo area messages")
    "h f" '(describe-function :wk "Describe function")
    "h F" '(describe-face :wk "Describe face")
    "h g" '(describe-gnu-project :wk "Describe GNU Project")
    "h i" '(info :wk "Info")
    "h I" '(describe-input-method :wk "Describe input method")
    "h k" '(describe-key :wk "Describe key")
    "h l" '(view-lossage :wk "Display recent keystrokes and the commands run")
    "h L" '(describe-language-environment :wk "Describe language environment")
    "h m" '(describe-mode :wk "Describe mode")
    "h r" '(:ignore t :wk "Reload")
    "h r r" 'my/reload-emacs
    ;; "h r r" '((lambda () (interactive)
    ;;             (load-file "~/.config/emacs/init.el")
    ;;             (ignore (elpaca-process-queues)))
    ;;           :wk "Reload emacs config")
    "h t" '(load-theme :wk "Load theme")
    "h v" '(describe-variable :wk "Describe variable")
    "h w" '(where-is :wk "Prints keybinding for command if set")
    "h x" '(describe-command :wk "Display full documentation for command"))

  (my/leader
    "m" '(:ignore t :wk "Org")
    "X" '(org-capture :wk "Org Capture")
    "m q" '(org-set-tags-command :wk "Set org tags for Heading")
    "m r r" '(org-refile :wk "Org regfile")
    "m a" '(org-agenda :wk "Org agenda")
    "m e" '(org-export-dispatch :wk "Org export dispatch")
    "m i" '(org-toggle-item :wk "Org toggle item")
    "m t" '(org-todo :wk "Org todo")
    "m B" '(org-babel-tangle :wk "Org babel tangle")
    "m T" '(org-todo-list :wk "Org todo list"))

  (my/leader
    "m b" '(:ignore t :wk "Tables")
    "m b -" '(org-table-insert-hline :wk "Insert hline in table"))

  (my/leader
    "m d" '(:ignore t :wk "Date/deadline")
    "m d t" '(org-time-stamp :wk "Org time stamp"))

  (my/leader
    "o" '(:ignore t :wk "Open")
    "o d" '(dashboard-open :wk "Dashboard")
    "o e" '(elfeed :wk "Elfeed RSS")
    "o f" '(make-frame :wk "Open buffer in new frame")
    "o A" '(org-agenda :wk "Org Agenda")
    "o F" '(select-frame-by-name :wk "Select frame by name"))

  ;; projectile-command-map already has a ton of bindings 
  ;; set for us, so no need to specify each individually.
  (my/leader
    "pp" '(project-switch-project :wk "Switch Project"))
  ;; "p" '(project-prefix-map :wk "Projectile"))
  
  ;; (my/leader
  ;;   "r" '(:ignore t :wk "Radio")
  ;;   "r p" '(eradio-play :wk "Eradio play")
  ;;   "r s" '(eradio-stop :wk "Eradio stop")
  ;;   "r t" '(eradio-toggle :wk "Eradio toggle"))


  ;; (my/leader
  ;;   "s" '(:ignore t :wk "Search")
  ;;   "s d" '(dictionary-search :wk "Search dictionary")
  ;;   "s m" '(man :wk "Man pages")
  ;;   "s o" '(pdf-occur :wk "Pdf search lines matching STRING")
  ;;   "s t" '(tldr :wk "Lookup TLDR docs for a command")
  ;;   "s w" '(woman :wk "Similar to man but doesn't require man"))
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  ;; (my/leader
  ;;   "t" '(:ignore t :wk "Toggle")
  ;;   "t e" '(eshell-toggle :wk "Toggle eshell")
  ;;   "t f" '(flycheck-mode :wk "Toggle flycheck")
  ;;   "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
  ;;   "t n" '(neotree-toggle :wk "Toggle neotree file viewer")
  ;;   "t o" '(org-mode :wk "Toggle org mode")
  ;;   "t r" '(rainbow-mode :wk "Toggle rainbow mode")
  ;;   "t t" '(visual-line-mode :wk "Toggle truncated lines")
  ;;   "t v" '(vterm-toggle :wk "Toggle vterm"))

  (my/leader
    "w" '(:ignore t :wk "Windows/Words")
    ;; Window splits
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split window")
    "w v" '(evil-window-vsplit :wk "Vertical split window")
    "w m m" '(delete-other-windows :wk "Delete all other windows")
    ;; Window motions
    "w h" '(evil-window-left :wk "Window left")
    "w j" '(evil-window-down :wk "Window down")
    "w k" '(evil-window-up :wk "Window up")
    "w l" '(evil-window-right :wk "Window right")
    "w w" '(evil-window-next :wk "Goto next window")
    ;; Move Windows
    "w H" '(buf-move-left :wk "Buffer move left")
    "w J" '(buf-move-down :wk "Buffer move down")
    "w K" '(buf-move-up :wk "Buffer move up")
    "w L" '(buf-move-right :wk "Buffer move right")
    ;; Words
    "w d" '(downcase-word :wk "Downcase word")
    "w u" '(upcase-word :wk "Upcase word")
    "w =" '(count-words :wk "Count words/lines for buffer"))
  )

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; TODO Why does this after-init hook not work as expected
  ;; :hook
  ;; (after-init . vertico-mode)
  :bind
  ( :map vertico-map
    ("DEL" . vertico-directory-delete-char))
  :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 22) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; :hook (after-init . marginalia-mode))
  :config (marginalia-mode))

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

(use-package org
  :ensure nil
  :config
  (add-hook 'org-capture-mode-hook 'evil-insert-state))

(use-package org-download
  :custom
  (org-download-image-dir (file-name-concat org-directory "blobs/org-download"))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;; Drag-and-drop to `dired`

(use-package org-sticky-header
  :custom
  (org-sticky-header-full-path 'full)
  :config

  (add-hook 'org-mode-hook 'org-sticky-header-mode))

;; Drag-and-drop to `dired`

(defun my/gtd-file (filename)
  (file-name-concat org-directory "gtd" filename))

(setq org-reverse-note-order t)
(setq org-src-preserve-indentation t)
(setq org-directory "~/Documents/org/")

(setq my-gtd-files (mapcar
		    #'my/gtd-file
		    '("next.org"
		      "read_review.org"
		      )))

(setq org-agenda-files (append
			(directory-files-recursively
			 (file-name-concat org-directory "Denotes/projects") "\\.org$")
			my-gtd-files
			))

(setq my-refile-files (append
		       org-agenda-files
		       (mapcar
			#'my/gtd-file
			'("someday.org"
			  "inbox_phone.org"
			  "Inbox.org"
			  ))))

(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
(setq org-inbox-file (file-truename (file-name-concat org-directory "gtd/Inbox.org")))
(setq org-refile-targets '((nil :maxlevel . 9)
			   (my-refile-files :maxlevel . 1)))
;; (directory-files-recursively org-directory "\\.org$" :maxlevel . 1)))

;; (("next.org"
;;  "read_review.org"
;;  "someday.org"
;;  ;; (org-refile-project-files :maxlevel . 1)
;;  "tickler.org"))))
;; (directory-files-recursively org-directory "Denotes\\.org$")

(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)

        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@creative" . ?c)
        ("@reading" .?b)
        ("@try" .?t)
        ("@media" .?m)
        ("@listening" .?l)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)
        ("@order" . ?o)))
(setq org-default-notes-file org-inbox-file)
(setq org-capture-templates
      '(("f" "Fleeting note" item
	 (file+headline org-default-notes-file "Notes")
	 "- %?")
	("p" "Permanent note" plain
	 (file denote-last-path)
	 #'denote-org-capture
	 :no-save t
	 :immediate-finish nil
	 :kill-buffer t
	 :jump-to-captured t)
	("t" "New task" entry
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %i%?")
	("K" "Cliplink capture task" entry
	 (file+headline org-default-notes-file "Links")
	 ;; "* TODO %(org-cliplink-capture) \n  SCHEDULED: %t\n" :empty-lines 1)))
	 "* TODO %(org-cliplink-capture)" :empty-lines 1)
        ("N" "New note with no prompts (with denote.el)" plain
	 (file denote-last-path)
	 (function
          (lambda ()
            (denote-org-capture-with-prompts nil nil nil)))
	 :no-save t
	 :immediate-finish nil
	 :kill-buffer t
	 :jump-to-captured t)
	("j" "Journal" entry
	 (file denote-journal-extras-path-to-new-or-existing-entry)
	 "* %U %?\n%i\n%a"
	 :kill-buffer t
	 :empty-lines 1)
	("P" "New project (with Denote)" plain
	 (file denote-last-path)
	 (function
	  (lambda ()
	    (let ((denote-use-directory (expand-file-name "projects" (denote-directory)))
		  ;; TODO Enable adding of additional keywords
		  (denote-use-keywords '("project"))
		  (denote-org-capture-specifiers (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
		  (denote-prompts (denote-add-prompts '(keywords)))

		  (denote-org-front-matter
		   (concat "#+title:      %s\n"
			   "#+date:       %s\n"
			   "#+filetags:   %s\n"
			   "#+identifier: %s\n"
			   "#+category: %1$s\n"
			   "\n")
		   ))
	      (denote-org-capture))))
	 :no-save t
	 :immediate-finish nil
	 :kill-buffer t
	 :jump-to-captured t)
	))

(add-hook 'org-mode-hook 'org-indent-mode)
(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(defun +org/dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- citation: follow it
- headline: cycle ARCHIVE subtrees, toggle latex fragments and inline images in
  subtree; update statistics cookies/checkboxes and ToCs.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- timestamp: open an agenda view for the time-stamp date/range at point.
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
;;- babel-call: execute the source block
- statistics-cookie: update it.
- src block: execute it
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        ((or `citation `citation-reference)
         (org-cite-follow context arg))

        (`headline
         (cond ((memq (bound-and-true-p org-goto-map)
                      (current-active-maps))
                (org-goto-ret))
               ((and (fboundp 'toc-org-insert-toc)
                     (member "TOC" (org-get-tags)))
                (toc-org-insert-toc)
                (message "Updating table of contents"))
               ((string= "ARCHIVE" (car-safe (org-get-tags)))
                (org-force-cycle-archived))
               ((or (org-element-property :todo-type context)
                    (org-element-property :scheduled context))
                (org-todo
                 (if (eq (org-element-property :todo-type context) 'done)
                     (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                         'todo)
                   'done))))
         ;; Update any metadata or inline previews in this subtree
         (org-update-checkbox-count)
         (org-update-parent-todo-statistics)
         (when (and (fboundp 'toc-org-insert-toc)
                    (member "TOC" (org-get-tags)))
           (toc-org-insert-toc)
           (message "Updating table of contents"))
         (let* ((beg (if (org-before-first-heading-p)
                         (line-beginning-position)
                       (save-excursion (org-back-to-heading) (point))))
                (end (if (org-before-first-heading-p)
                         (line-end-position)
                       (save-excursion (org-end-of-subtree) (point))))
                (overlays (ignore-errors (overlays-in beg end)))
                (latex-overlays
                 (cl-find-if (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-latex-overlay))
                             overlays))
                (image-overlays
                 (cl-find-if (lambda (o) (overlay-get o 'org-image-overlay))
                             overlays)))
           (+org--toggle-inline-images-in-subtree beg end)
           (if (or image-overlays latex-overlays)
               (org-clear-latex-preview beg end)
             (org--latex-preview-region beg end))))

        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        ((or `table `table-row)
         (if (org-at-TBLFM-p)
             (org-table-calc-current-TBLFM)
           (ignore-errors
             (save-excursion
               (goto-char (org-element-property :contents-begin context))
               (org-call-with-arg 'org-table-recalculate (or arg t))))))

        (`table-cell
         (org-table-blank-field)
         (org-table-recalculate arg)
         (when (and (string-empty-p (string-trim (org-table-get-field)))
                    (bound-and-true-p evil-local-mode))
           (evil-change-state 'insert)))

        ;; (`babel-call
        ;;  (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ;; ((or `src-block `inline-src-block)
        ;;  (org-babel-execute-src-block arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (+org--toggle-inline-images-in-subtree
                (org-element-property :begin lineage)
                (org-element-property :end lineage))
             (org-open-at-point arg))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (org-toggle-checkbox))

        (`paragraph
         (+org--toggle-inline-images-in-subtree))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           (+org--toggle-inline-images-in-subtree
            (org-element-property :begin context)
            (org-element-property :end context))))))))

(defun +org--toggle-inline-images-in-subtree (&optional beg end refresh)
  "Refresh inline image previews in the current heading/tree."
  (let* ((beg (or beg
                  (if (org-before-first-heading-p)
                      (save-excursion (point-min))
                    (save-excursion (org-back-to-heading) (point)))))
         (end (or end
                  (if (org-before-first-heading-p)
                      (save-excursion (org-next-visible-heading 1) (point))
                    (save-excursion (org-end-of-subtree) (point)))))
         (overlays (cl-remove-if-not (lambda (ov) (overlay-get ov 'org-image-overlay))
                                     (ignore-errors (overlays-in beg end)))))
    (dolist (ov overlays nil)
      (delete-overlay ov)
      (setq org-inline-image-overlays (delete ov org-inline-image-overlays)))
    (when (or refresh (not overlays))
      (org-display-inline-images t t beg end)
      t)))

(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

(general-define-key
 :keymaps 'org-mode-map
 :states 'normal
 "RET" #'+org/dwim-at-point
 )

(defun +org/shift-return (&optional arg)
  "Insert a literal newline, or dwim in tables.
Executes `org-table-copy-down' if in table."
  (interactive "p")
  (if (org-at-table-p)
      (org-table-copy-down arg)
    (org-return nil arg)))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal insert)
 "S-<return>" #'+org/shift-return
 )

(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (run-hooks 'org-insert-heading-hook)
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))


;; I use these instead of `org-insert-item' or `org-insert-heading' because they
;; impose bizarre whitespace rules depending on cursor location and many
;; settings. These commands have a much simpler responsibility.
(defun +org/insert-item-below (count)
;; (defun +org/insert-item-below ()
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  ;; (+org--insert-item 'below))
  (dotimes (_ count) (+org--insert-item 'below)))

(defun +org/insert-item-above (count)
;; (defun +org/insert-item-above ()
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  ;; (+org--insert-item 'above))
  (dotimes (_ count) (+org--insert-item 'above)))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal insert)

 "C-<return>" #'+org/insert-item-below
 "C-S-<return>" #'+org/insert-item-above
 "C-M-<return>" #'org-insert-subheading
 )

(use-package org-cliplink
  :config
  (my/leader "mlc" 'org-cliplink))
;; :after general
;; :general

(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

(use-package denote
  :after org
  :config
  (setq denote-directory (file-truename (file-name-concat org-directory "Denotes/"))))
;; (with-eval-after-load 'org-capture
(add-to-list 'org-capture-templates
             '("N" "New note with no prompts (with denote.el)" plain
	       (file denote-last-path)
	       (function
                (lambda ()
                  (denote-org-capture-with-prompts nil nil nil)))
	       :no-save t
	       :immediate-finish nil
	       :kill-buffer t
	       :jump-to-captured t))
(add-to-list 'org-capture-templates
             '("j" "Journal" entry
               (file denote-journal-extras-path-to-new-or-existing-entry)
               "* %U %?\n%i\n%a"
               :kill-buffer t
               :empty-lines 1))
;; TODO Add hook to automatically add the new file to agenda, until then, just reload config
(add-to-list 'org-capture-templates
	     '("P" "New project (with Denote)" plain
	       (file denote-last-path)
	       (function
		(lambda ()
                  (let ((denote-use-directory (expand-file-name "projects" (denote-directory)))
			;; TODO Enable adding of additional keywords
			(denote-use-keywords '("project"))
			(denote-org-capture-specifiers (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
			(denote-prompts (denote-add-prompts '(keywords)))

			(denote-org-front-matter
			 (concat "#+title:      %s\n"
				 "#+date:       %s\n"
				 "#+filetags:   %s\n"
				 "#+identifier: %s\n"
				 "#+category: %1$s\n"
				 "\n")
			 ))
		    (denote-org-capture))))
	       :no-save t
	       :immediate-finish nil
	       :kill-buffer t
	       :jump-to-captured t))
;; )
(defun my-denote-region-org-structure-template (_beg _end)
  (when (derived-mode-p 'org-mode)
    (activate-mark)
    (call-interactively 'org-insert-structure-template)))

;; TODO Maybe also add a link to the source?
(add-hook 'denote-region-after-new-note-functions #'my-denote-region-org-structure-template)


;; Variant of `my-denote-region' to reference the source

(defun my-denote-region-get-source-reference ()
  "Get a reference to the source for use with `my-denote-region'.
The reference is a URL or an Org-formatted link to a file."
  ;; We use a `cond' here because we can extend it to cover move
  ;; cases.
  (cond
   ((derived-mode-p 'eww-mode)
    (plist-get eww-data :url))
   ;; Here we are just assuming an Org format.  We can make this more
   ;; involved, if needed.
   (buffer-file-name
    (format "[[file:%s][%s]]" buffer-file-name (buffer-name)))))

(defun my-denote-region ()
  "Like `denote-region', but add the context afterwards.
For how the context is retrieved, see `my-denote-region-get-source-reference'."
  (interactive)
  (let ((context (my-denote-region-get-source-reference)))
    (call-interactively 'denote-region)
    (when context
      (goto-char (point-max))
      (insert "\n")
      (insert context))))

;; Add quotes around snippets of text captured with `denote-region' or `my-denote-region'.

(defun my-denote-region-org-structure-template (beg end)
  "Automatically quote (with Org syntax) the contents of `denote-region'."
  (when (derived-mode-p 'org-mode)
    (goto-char end)
    (insert "#+end_quote\n")
    (goto-char beg)
    (insert "#+begin_quote\n")))

(add-hook 'denote-region-after-new-note-functions #'my-denote-region-org-structure-template)

(defun file-to-string (file)
  "File to string function"
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;; (defun my-denote-template (template-file-name)
;;   (lambda ()
;;     (file-to-string (file-name-concat user-emacs-directory "templates" template-file-name))))
;; ;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(defun my-weekly-review-template ()
  ;; (interactive)
  (file-to-string "~/Documents/org/gtd/templates/weekly_review.org"))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(defun my-daily-journal-template ()
  ;; (interactive)
  (file-to-string "templates/daily_journal.org"))

(defun my-project-template ()
  ;; (interactive)
  (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))
;; (file-to-string((file-truename (file-name-concat org-directory "gtd/templates/weekly_review.txt")))))

(setq denote-templates '((weekly_review . my-weekly-review-template)
			 (daily_journal . my-daily-journal-template)
			 (project . my-project-template)
			 ;; (theproject . (my-denote-template "project.org"))
			 ))

;; (message (file-to-string "~/Documents/org/gtd/templates/weekly_review.txt"))

(use-package citar
  :custom
  (citar-bibliography '("~/Documents/org/bib/references.bib")))


;; Biblio package for adding BibTeX records and download publications
(use-package biblio)

(use-package org-ref)
;; (require 'org-ref-url-utils)

(use-package citar-denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode))
;; :bind
;; (("C-c w b c" . citar-create-note)
;;  ("C-c w b n" . citar-denote-open-note)
;;  ("C-c w b x" . citar-denote-nocite)
;;  :map org-mode-map
;;  ("C-c w b k" . citar-denote-add-citekey)
;;  ("C-c w b K" . citar-denote-remove-citekey)
;;  ("C-c w b d" . citar-denote-dwim)
;;  ("C-c w b e" . citar-denote-open-reference-entry)))

(use-package magit)
