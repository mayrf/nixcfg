;; -*- lexical-binding: t; -*-

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

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t  ;; This is optional since it's already set to t by default.
        evil-want-keybinding nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-undo-system 'undo-redo)  ;; Adds vim-like C-r redo functionality
  (evil-mode))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

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

(use-package load-env-vars
  :config
  (load-env-vars (file-name-concat user-emacs-directory ".env")))

(defun org-babel-tangle-config ()
  ;; (when (string-equal (buffer-file-name)
   ;; 		      (expand-file-name "~/.config/emacs/mayrf-emacs.org"))
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
  ;; (org-babel-tangle "~/.config/emacs/mayrf-emacs.org")
  (my/reload-init-el))
  ;; (my/reload-modules))

(defun my/reload-init-el ()
  (load-file "~/.config/emacs/init.el"))


(defun my/reload-modules ()
  (interactive)
  (load-directory (locate-user-emacs-file "mayrf-emacs-modules")))
;; (mapc
;;  (lambda (string)
;;    (add-to-list 'load-path (locate-user-emacs-file string)))
;;'("prot-lisp" "prot-emacs-modules"))
;;'("mayrf-lisp" "mayrf-emacs-modules"))

(global-visual-line-mode t)
(which-key-mode)
(add-to-list 'default-frame-alist '(alpha-background . 70)) ; For all new frames henceforth

(setq visible-bell t)

(define-key minibuffer-local-map (kbd "C-v") 'yank)

;; (set-frame-font "iMWritingMono Nerd Font" nil t)
;; (set-frame-font "JetBrainsMono Nerd Font,JetBrainsMono NF" nil t)
;; (set-frame-font "JetBrainsMono Nerd Font" nil t)
;; (set-frame-font "CaskaydiaCove Nerd Font" nil t)
(set-face-attribute 'default nil
  ;; :font "GeistMono Nerd Font"
  :font "JetBrainsMono Nerd Font"
  :height 100
  :weight 'medium)
;; (set-face-attribute 'variable-pitch nil
;;   :font "Ubuntu Nerd Font"
;;   :height 120
;;   :weight 'medium)
;; (set-face-attribute 'fixed-pitch nil
;;   :font "Source Code Pro"
;;   :height 110
;;   :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
;; (add-to-list 'default-frame-alist '(font . "GeistMono Nerd Font-11"))
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10"))

;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

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

(use-package general
  :ensure (:wait t)
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
    "c" '(:ignore t :wk "Code")    
    "c n" '(compile :wk "Compile")
    "c c" '(recompile :wk "Recompile"))
  (my/leader
    "f" '(:ignore t :wk "Files")    
    "SPC" '(project-find-file :wk "Find File in Project")
    "f P" '((lambda () (interactive) (find-file "~/.config/emacs/mayrf-emacs.org")) :wk "Open Config")
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
    "f o" '(consult-outline :wk "Consult outline")
    "f r" '(consult-recent-file :wk "Find recent files")
    "/" '(consult-git-grep :wk "Grep for a file in project or DIR") ;; changes from consult-grep and this wouldn't respect .gitingore
    "f b" '(consult-buffer :wk "Consult buffer")
    ;; "f u" '(sudo-edit-find-file :wk "Sudo find file")
    ;; "f U" '(sudo-edit :wk "Sudo edit file"))
    )
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
    "m a" '(org-archive-subtree :wk "Archive org subtree")
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
    "m d s" '(org-schedule :wk "Org schedule")
    "m d d" '(org-deadline :wk "Org dealine")
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

(use-package vterm
  :ensure nil
  :config
  (defun my/vterm-in-parent-directory ()
    "Open vterm and change to the parent directory of current buffer."
    (interactive)
    (let ((parent-dir (file-name-directory (or (buffer-file-name) default-directory))))
      (vterm)
      ;; Clear any existing input first
      (vterm-send-key "u" nil nil t) ;; Ctrl+u to clear the line
      (vterm-send-string (concat "cd " (shell-quote-argument parent-dir)))
      (vterm-send-return)))
  (my/leader
    " o t" '(my/vterm-in-parent-directory :wk "open vterm and cd to dir of current buffer")
    " o T" '(vterm :wk "open vterm"))
  )
;; :load-path  "path/to/emacs-libvterm/")

(use-package dired-open
  :config
  (evil-define-key 'normal dired-mode-map (kbd "h") 'dired-up-directory)
  (evil-define-key 'normal dired-mode-map (kbd "l") 'dired-open-file) ; use dired-find-file instead if not using dired-open package
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

(use-package nerd-icons
  :custom
  ;; (nerd-icons-font-family "iMWritingMono Nerd Font")
  (nerd-icons-font-family "Symbols Nerd Font Mono")
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
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  )

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  ;; :hook (after-init . marginalia-mode))
  :config (marginalia-mode))


(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  ;; :ensure t ; only need to install it, embark loads it after consult if found
  :bind
  (("C-;" . embark-export))         ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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
  :custom

  ;; Org Export Settings
  (org-directory "~/Documents/org/")
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-num nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%d %B %Y")

  (org-reverse-note-order t)
  (org-src-preserve-indentation t)

  (org-tag-alist
   '(
     ;; (:startgrouptag . nil)
     (:startgroup)
     ("Contexts")
     (:grouptags)
     ("@computer" . ?c)
     ("@phone" . ?m)
     ("@digital" . ?d)
     ("@errands" . ?e)
     ("@event" . ?E)
     (:endgrouptag)

     
     (:startgroup)
     ("Areas")
     (:grouptags)
     ("@home" . ?h)
     ("@work" . ?w)
     (:endgroup)


     ;; Task Types
     (:startgrouptag . nil)
     ("Types")
     (:grouptags)
     ("@programming" . ?p)
     ("@creative" . ?C)
     ("@reading" .?r)
     ("@media" .?m)
     ("@listening" .?l)
     ("@try" .?t)
     ("@email" . ?M)
     ("@calls" . ?a)
     ("@person" . ?s)
     ("@planning" . ?n)
     ;; ("@easy" . ?e)
     ;; ("@hacking" . ?h)
     ;; ("@writing" . ?w)
     ;; ("@accounting" . ?a)
     ;; ("@system" . ?s)
     ;; ("@order" . ?o)
     (:endgrouptag)

     ))


  :ensure nil
  :init
  (add-hook 'org-mode-hook (lambda ()
                             ;; (fset 'tex-font-lock-suscript 'ignore)
                             (org-babel-do-load-languages
                              'org-babel-load-languages
                              '((python . t)
                                (shell . t)))))

  :config


  (defun my/gtd-file (filename)
    (file-name-concat org-directory "gtd" filename))


  (setq my-gtd-files (mapcar
                      #'my/gtd-file
                      '("next.org"
                        "read_review.org"
                        )))

  (setq org-agenda-files
	(mapcar
	 #'my/gtd-file
	 '(
	   "next.org"
	   "agenda.org"
	   )
	 )
	)

  (setq my-refile-files (append
                         org-agenda-files
                         (mapcar
                          #'my/gtd-file
                          '("someday.org"
                            "inbox_phone.org"
                            "read_review.org"
                            "Inbox.org"
                            ))))

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-inbox-file (file-truename (file-name-concat org-directory "gtd/Inbox.org")))
  (setq org-next-file (file-truename (file-name-concat org-directory "gtd/next.org")))
  (setq org-refile-targets `(
                             (,my-refile-files :maxlevel . 1)))

  (setq org-default-notes-file org-inbox-file)
  (setq org-capture-templates
        '(("f" "Fleeting note" item
           (file+headline org-default-notes-file "Notes")
           "- %?")
          ;; ("p" "Permanent note" plain
          ;;  (file denote-last-path)
          ;;  #'denote-org-capture
          ;;  :no-save t
          ;;  :immediate-finish nil
          ;;  :kill-buffer t
          ;;  :jump-to-captured t)
          ;; ("t" "New task" entry
          ;;  (file+headline org-default-notes-file "Tasks")
          ;;  "* TODO %i%?")
          ("t" "todo" entry 
           (file+headline org-next-file "SIMPLE TASKS")
           "* TODO %?")
          ("T" "todo today" entry 
           (file+headline org-next-file "SIMPLE TASKS")
           "* TODO %?\nDEADLINE: %t")
          ("i" "inbox" entry 
           (file "~/inbox.org")
           "* %?")
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
  (add-hook 'org-capture-mode-hook 'evil-insert-state)
  (setq org-src-fontify-natively t))

(general-define-key
 :keymaps 'org-mode-map
 :states '(normal visual insert)
 "M-h" #'org-metaleft
 "M-l" #'org-metaright
 "M-j" #'org-metadown
 "M-k" #'org-metaup
 )

(with-eval-after-load 'org
  (add-to-list 'org-file-apps '("\\.odt\\'" . "libreoffice %s")))

(defun my/run-shell-command-in-dir (directory command)
  "Run a shell COMMAND in the specified DIRECTORY and display output in a popup buffer."
  (interactive
   (list
    (read-directory-name "Directory: ")
    (read-shell-command "Shell command: ")))
  (let ((default-directory (file-name-as-directory (expand-file-name directory)))
        (buffer-name "*Shell Command Output*"))
    (with-output-to-temp-buffer buffer-name
      (with-current-buffer buffer-name
        (let ((exit-code (call-process-shell-command command nil t t)))
          (insert (format "\n\n[Process exited with code %d]" exit-code)))))))

(defun my/run-git-sync ()
  "Run a shell COMMAND in the specified DIRECTORY and display output in a popup buffer."
  (interactive)
  (my/run-shell-command-in-dir org-directory "git-sync -n"))

  (my/leader
    "m s g" '(my/run-git-sync :wk "Sync org files")
    )

(use-package olivetti)

(use-package org-download
  :after org
  :custom
  (org-download-image-dir (file-name-concat org-directory "blobs/org-download"))
  :config
  (add-hook 'dired-mode-hook 'org-download-enable))

;; Drag-and-drop to `dired`

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package org-sticky-header
  :after org
  :custom
  (org-sticky-header-full-path 'full)
  :config
  (add-hook 'org-mode-hook 'org-sticky-header-mode)
  )

;; Drag-and-drop to `dired`

(use-package org-cliplink
  :config
  (my/leader "mlc" 'org-cliplink))

(use-package org
  :ensure nil
  :config
  (defun my/vterm-execute-current-line ()
    "Insert text in vterm and execute.
   If region is active, execute the selected text.
   Otherwise, execute current line and any continuation lines marked with backslash."
    (interactive)
    (require 'vterm)
    (eval-when-compile (require 'subr-x))
    (let ((command
           (if (use-region-p)
               ;; Use the selected region
               (string-trim (buffer-substring (region-beginning) (region-end)))
             ;; No region, so get current line and any continuation lines
             (let ((start-point (save-excursion
                                  (beginning-of-line)
                                  (point)))
                   (end-point nil))
               (save-excursion
		 (beginning-of-line)
		 (while (and (not (eobp))
                             (or (not end-point)
				 (and (> (point) start-point)
                                      (save-excursion
					(end-of-line 0)  ; Move to end of previous line
					(looking-back "\\\\" (- (point) 1))))))
                   (end-of-line)
                   (setq end-point (point))
                   (unless (eobp) (forward-line 1)))
		 (string-trim (buffer-substring start-point end-point)))))))
      (let ((buf (current-buffer)))
	(unless (get-buffer vterm-buffer-name)
          (vterm))
	(display-buffer vterm-buffer-name t)
	(switch-to-buffer-other-window vterm-buffer-name)
	(vterm--goto-line -1)
	(message command)
	(vterm-send-string command)
	(vterm-send-return)
	(switch-to-buffer-other-window buf))))

  (my/leader
    "m b t" '(my/vterm-execute-current-line :wk "Send and execute region/line to vterm")
    ))

(setq org-src-preserve-indentation t)

(setq org-src-tab-acts-natively t)

;; (use-package org-caldav
;;   :config
;;   (setq org-caldav-url "https://<nextcloudURL>/remote.php/dav/calendars/<CalenderName>")
;;   ;; calendar ID on server
;;   (setq org-caldav-calendar-id "personal")
;;   ;; Org filename wherech new entries from calendar stored
;;   (setq org-caldav-inbox "~/Documents/org/nextcloud-inbox.org")
;;   ;; Additional Org files to check for calendar events
;;   (setq org-caldav-files nil)
;;   ;; Usually a good idea to set the timezone manually
;;   (setq org-icalendar-timezone "Europe/Berlin")
;;   :commands (org-caldav-sync))

(defun my/collect-org-tag-combinations (files)
  "Collect all unique tag combinations from org FILES."
  (let ((tag-combinations '())
        (file-count 0)
        (total-files (length files)))
    (dolist (file files)
      (setq file-count (1+ file-count))
      (message "Processing file %d/%d: %s" file-count total-files (file-name-nondirectory file))
      
      (when (file-readable-p file)
        (let ((buffer-was-open (get-file-buffer file))
              (buffer nil))
          (condition-case err
              (progn
                (setq buffer (find-file-noselect file))
                (with-current-buffer buffer
                  (when (derived-mode-p 'org-mode)
                    (org-map-entries
                     (lambda ()
                       (let ((tags (org-get-tags)))
                         (when tags
                           (setq tags (sort (copy-sequence tags) 'string<))
                           (unless (member tags tag-combinations)
                             (push tags tag-combinations))))))))
                
                ;; Clean up: kill buffer only if we opened it
                (when (and buffer (not buffer-was-open))
                  (kill-buffer buffer)))
            
            (error
             (message "Error processing file %s: %s" file (error-message-string err))
             (when (and buffer (not buffer-was-open))
               (ignore-errors (kill-buffer buffer))))))))
    
    (message "Found %d unique tag combinations" (length tag-combinations))
    tag-combinations))

(defun my/sort-tag-combinations (tag-combinations)
  "Sort tag combinations by number of tags, then alphabetically."
  (sort tag-combinations
        (lambda (a b)
          (if (= (length a) (length b))
              (string< (string-join a "+") (string-join b "+"))
            (< (length a) (length b))))))

(defun my/create-agenda-commands (tag-combinations files)
  "Create agenda command list for tag combinations."
  (let ((commands '()))
    ;; Add commands for each tag combination
    (dolist (tags tag-combinations)
      (let ((tag-string (string-join tags "+"))
            (header (format "🏷️ %s" (string-join tags " + "))))
        (push `(tags-todo ,tag-string
                         ((org-agenda-overriding-header ,header)
                          (org-agenda-files ',files)))
              commands)))
    
    ;; Add section for items with no tags
    (push `(tags-todo "-{.*}"
                     ((org-agenda-overriding-header "📝 No Tags")
                      (org-agenda-files ',files)))
          commands)
    
    (reverse commands)))

(defun my/org-gtd-agenda-by-tag-composition ()
  "Create agenda view grouped by exact tag combinations."
  (interactive)
  (let* ((files (directory-files-recursively "~/Documents/org/gtd" "\\.org$"))
         (tag-combinations (my/collect-org-tag-combinations files))
         (sorted-combinations (my/sort-tag-combinations tag-combinations))
         (commands (my/create-agenda-commands sorted-combinations files))
         (org-agenda-custom-commands
          `(("x" "Dynamic Tag Composition View"
             ,commands
             ((org-agenda-sorting-strategy '(priority-down))
              (org-agenda-prefix-format "  %-12:c %?-12t% s"))))))
    (org-agenda nil "x")))
(defun my/org-agenda-by-tag-composition ()
  "Create agenda view grouped by exact tag combinations."
  (interactive)
  (let* ((files org-agenda-files)
         (tag-combinations (my/collect-org-tag-combinations files))
         (sorted-combinations (my/sort-tag-combinations tag-combinations))
         (commands (my/create-agenda-commands sorted-combinations files))
         (org-agenda-custom-commands
          `(("x" "Dynamic Tag Composition View"
             ,commands
             ((org-agenda-sorting-strategy '(priority-down))
              (org-agenda-prefix-format "  %-12:c %?-12t% s"))))))
    (org-agenda nil "x")))

;; Bind to a key for easy access
;; (global-set-key (kbd "C-c a t") 'my/org-agenda-by-tag-composition)

;; Optional: Set up org-agenda-files to include all org files in ~/org
;; (setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))

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

(use-package notmuch)

(use-package denote
  :after org
  :config
  (my/leader
    "n r f" '(denote-open-or-create :wk "Open or create note")
    "n r i" '(denote-link-or-create :wk "Link or create to note")
    "n r R" '(denote-rename-file-using-front-matter :wk "Rename note using front matter"))
  (setq denote-directory (file-truename (file-name-concat org-directory "Denotes/"))))
;; (with-eval-after-load 'org-capture
;; (add-to-list 'org-capture-templates
;;              '("N" "New note with no prompts (with denote.el)" plain
;; 	       (file denote-last-path)
;; 	       (function
;;                 (lambda ()
;;                   (denote-org-capture-with-prompts nil nil nil)))
;; 	       :no-save t
;; 	       :immediate-finish nil
;; 	       :kill-buffer t
;; 	       :jump-to-captured t))
;; (add-to-list 'org-capture-templates
;;              '("j" "Journal" entry
;;                (file denote-journal-extras-path-to-new-or-existing-entry)
;;                "* %U %?\n%i\n%a"
;;                :kill-buffer t
;;                :empty-lines 1))
;; ;; TODO Add hook to automatically add the new file to agenda, until then, just reload config
(add-to-list 'org-capture-templates
	     '("P" "New project (with Denote)" plain
	       (file denote-last-path)
	       (function
		(lambda ()
                  (let ((denote-use-directory (expand-file-name "projects" (denote-directory)))
			;; TODO Enable adding of additional keywords
			(denote-use-keywords '("pra"))
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

(use-package consult-denote
  :after denote
  :config
  (consult-denote-mode)
  (setq consult-async-min-input 0)
  ;; (my/leader
  ;;   "n r f" '(consult-denote-find :wk "Find denote note"))
)

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

(defvar my/denote-keyword-alist
  '(
    ( “pra” . "Active Project" )
    ( “prb” . "Backlogged Project" )
    ( “prc” . "Completed Project" )
    ( “ply” . "Planning yearly" )
    ( “plm” . "Planning monthly" )
    ( “plw” . "Planning weekly" )
    ;; ( “kh” . "kind ..." )
    ( “kn” . "kind Note" )
    ( “kt” . "kind Topic" )
    ( “kp” . "kind Person" )
    ;; ( “kl” . "kind ..." )
    ;; ( “ka” . "kind ..." )
    ;; ( “kap” . "" )
    ;; ( “kcp” . "" )
    ;; ( “kca” . "" )
    ;; ( “kcc” . "" )
    ( “kra” . "Kind reference Article" )
    ( “krb” . "Kind reference Book" )
    ( “krv” . "Kind reference Video" )
    ;; ( “rn” . "" )
    ))

(setq denote-infer-keywords
      nil
      denote-known-keywords
      (mapcar #'car my/denote-keyword-alist))

;;; ----- Time Tracking -----

;; ;; Clock in on the current task when setting a timer
;; (add-hook 'org-timer-set-hook #'org-clock-in)

;; ;; Clock out of the current task when the timer is complete
;; (add-hook 'org-timer-done-hook #'org-clock-out)

;;; ----- Agenda Configuration -----

(defun my/gtd-file (filename)
  (file-name-concat org-directory "gtd" filename))

(setq org-reverse-note-order t)
(setq org-src-preserve-indentation t)
(setq org-directory "~/Documents/org/")

(setq org-agenda-files
      ;; (append (denote-directory-files "search term")
	      (mapcar
	       #'my/gtd-file
	       '(
		 "next.org"
		 "agenda.org"
		 ;; "read_review.org"
		 ;; "projects.org"
		 ))
	      ;; )
      )


      (setq org-agenda-span 'day
	    org-agenda-start-with-log-mode t
	    org-agenda-window-setup 'current-window)

      ;; Make done tasks show up in the agenda log
      (setq org-log-done 'time
	    org-log-into-drawer t)

;;; ----- Denote Integration -----

      (defun my/refresh-agenda-files ()
	(interactive)
	(setq org-agenda-files
              (append (denote-directory-files "_pra")
                      org-agenda-files)))

      (defun my/goto-weekly-note ()
	(interactive)
	(let* ((note-title (format-time-string "%Y-W%V"))
               (existing-notes
		(denote-directory-files (format "-%s" note-title) nil t)))
	  (if existing-notes
              (find-file (car existing-notes))
	    (denote note-title '("plw")))))

      ;; TODO Automatically use weekly review template
      (defun my/goto-weekly-review ()
	(interactive)
	(let* (
	       (note-title (concat (format-time-string "%Y-W%V") "-weekly-review"))
               ;; (denote-org-capture-specifiers (file-to-string (file-name-concat user-emacs-directory "templates/project.org")))
               (existing-notes
		(denote-directory-files (format "-%s" note-title) nil t)))
	  (if existing-notes
              (find-file (car existing-notes))
	    (denote note-title '("plw") 'org my-weekly-review-template))))

      (with-eval-after-load 'denote
	;; Quick access commands
	(keymap-set global-map "C-c n w" #'my/goto-weekly-note)
	(my/leader
	  ;; "SPC" '(counsel-M-x :wk "Counsel M-x")
	  "n w r" '(my/goto-weekly-review :wk "Go to weekly review note")
	  "n w w" '(my/goto-weekly-note :wk "Go to weekly note"))

	;; Refresh agenda files the first time
	(my/refresh-agenda-files)

	;; Update agenda files after notes are created or renamed
	(add-hook 'denote-after-rename-file-hook #'my/refresh-agenda-files)
	(add-hook 'denote-after-new-note-hook #'my/refresh-agenda-files))

;; ;; Workflow states
;; (:startgroup . nil)
;; ("States")
;; ("@plan" . ?p)
;; ("@review" . ?r)
;; ("@followup" . ?f)

;; Only make context tags inheritable (what about noexport?)
;; (setq org-use-tag-inheritance "^@")

(setq org-agenda-custom-commands
      '(
	;; ("p" "Planning" tags-todo "@planning")
	;; ("p" "Planning"
        ;;  ((tags-todo "+@planning")
        ;;   (tags-todo "-{.*}")))
	("p" "Planning"
         ((tags-todo "+@planning"
                     ((org-agenda-overriding-header "Planning Tasks")))
          (tags-todo "-{.*}"
                     ((org-agenda-overriding-header "Untagged Tasks")))
          (todo ".*" ((org-agenda-files (mapcar
					 #'my/gtd-file
					 '( "Inbox.org" "inbox_phone.org")))
                      ;; (org-agenda-overriding-header "Unprocessed Inbox Items")
		      ))))

	("n" "next" todo "NEXT")
        ("u" "untagged Tasks" tags-todo "-{.*}")
        ("i" "Inbox"
         ((todo "" ((org-agenda-files '("~/Documents/org/gtd/Inbox.org"))
                    ;; (org-agenda-overriding-header "Unprocessed Inbox Items")
		    ))))

	("d" "Daily Agenda"
         ((agenda "" ((org-agenda-span 'day)
                      (org-deadline-warning-days 7)))
          (tags-todo "+PRIORITY=\"A\""
                     ((org-agenda-overriding-header "High Priority Tasks")))
          (todo "NEXT"
                     ((org-agenda-overriding-header "Next Tasks")))
	  ))
	("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-overriding-header "Completed Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
                   (org-agenda-span 'week)))

          (agenda ""
                  ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-span 'week)))))
	
	))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "PROJ(p)" "|" "DONE(d)" "CANCELED(c)")
        ))

(setq org-stuck-projects
      '("+TODO=\"PROJ\"" ("NEXT") nil "") )

(defun my/org-promote-next-todo-to-next ()
  "Promote the next TODO item to NEXT if the current item is set to DONE."
  (when (string= org-state "DONE")
    (save-excursion
      (when (org-next-visible-heading 1)
        (let ((current-heading (thing-at-point 'line)))
          (when (string-match "^\\*+ +TODO " current-heading)
            (replace-regexp-in-string "TODO" "NEXT" current-heading nil 'literal)
            (org-todo "NEXT")))))))

(add-hook 'org-after-todo-state-change-hook 'my/org-promote-next-todo-to-next)

(defun my/org-demote-next-next-to-todo ()
  "Promote the next TODO item to NEXT if the current item is set to DONE."
  (when (string= org-state "TODO")
    (save-excursion
      (when (org-next-visible-heading 1)
        (let ((current-heading (thing-at-point 'line)))
          (when (string-match "^\\*+ +NEXT " current-heading)
            (replace-regexp-in-string "NEXT" "TODO" current-heading nil 'literal)
            (org-todo "TODO")))))))

(add-hook 'org-after-todo-state-change-hook 'my/org-demote-next-next-to-todo)

(defun my/org-add-category-from-proj ()
  "Add a :CATEGORY: property if TODO keyword is PROJ, using the rest of the heading."
  (interactive)
  (when (org-at-heading-p)
    (let* ((todo (org-get-todo-state))
	   (category (org-get-category))
           (headline (nth 4 (org-heading-components))))
      (if
	  (and todo (string= todo "PROJ"))
          (org-set-property "CATEGORY" headline)
	(when (and category (string= category headline))
	  (org-delete-property "CATEGORY")
	  )
	))))

(add-hook 'org-after-todo-state-change-hook 'my/org-add-category-from-proj)

(add-to-list
 'org-capture-templates
 '("p" "Project" entry
   (file org-next-file)
   "* PROJ %^{Brief Description}\n:PROPERTIES:\n:CATEGORY: %^{Id}\n:END:\nAdded: %U\n%?" :empty-lines 1 :prepend t))

(use-package transient)

(use-package magit
  :after transient
  :config
  (defun my/magit-soft-reset-head~1 ()
    "Soft reset current git repo to HEAD~1."
    (interactive)
    (magit-reset-soft "HEAD~1"))
  ;; Add additional switches that seem common enough
  (transient-append-suffix 'magit-fetch "-p"
    '("-t" "Fetch all tags" ("-t" "--tags")))
  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))
  )

;; (use-package forge
;;   :after magit
;;   :config
;;   (setq auth-sources '("~/.authinfo")
;; 	work-gitforge-host (getenv "WORK_GITFORGE_HOST"))
;;   (add-to-list 'forge-alist `( ,work-gitforge-host                       ; GITHOST
;; 			       ,(concat work-gitforge-host "/api/v4")                ; APIHOST
;; 			       ,work-gitforge-host                       ; WEBHOST and INSTANCE-ID
;; 			       forge-gitlab-repository)           ; CLASS
;; 	       )
;;   )

(use-package gptel
  :config
  (setq gptel-model 'llama3.1:latest
	gptel-backend (gptel-make-ollama "Ollama"
			:host "localhost:11434"
			:stream t
			:models '(llama3.1:latest)))
  )
;; (gptel-make-ollama "Ollama"             ;Any name of your choosing
;;   :host "localhost:11434"               ;Where it's running
;;   :stream t                             ;Stream responses
;;   :models '(llama3.1:latest))          ;List of models

(use-package eglot
  :ensure nil 
  :config
  (my/leader
    "l" '(:ignore t :wk "Org")
    "l f" '(eglot-format :wk "format buffer")
    "l r" '(eglot-rename :wk "rename symbol")
    "l a" '(eglot-code-actions :wk "lsp code actions")
    ))

(use-package blamer
  :ensure t
  :bind (("s-i" . blamer-show-commit-info)
         ("C-c i" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    ;; :background nil
                    ;; :height 100
                    :italic t)))
  :config
  (global-blamer-mode 0))

(use-package yasnippet 
  :config
  (setq yas-snippet-dirs
	'("~/.config/emacs/snippets"                 ;; personal snippets
          ;; "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
          ;; "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
          ))

  (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
  )
;; (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
;; (yas-global-mode 1))

;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'eglot-format nil t)))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (kcl "https://github.com/kcl-lang/tree-sitter-kcl")
     (just "https://github.com/IndianBoy42/tree-sitter-just")
     ))

;; TODO Find a way to have grammars installed declarativly
(use-package treesit-auto
  :custom
  ;; (treesit-auto-install 'prompt)
  (treesit-auto-install t)
  :config
  ;; (setq treesit-auto-langs '(javascript typescript tsx css html))
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;; (treesit-auto-add-to-auto-mode-alist '(javascript typescript tsx css html))
  (global-treesit-auto-mode))

(electric-pair-mode)

(use-package direnv
 :config
 (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package go-ts-mode
  :ensure nil
  :mode ("\\.go\\'" . go-ts-mode)
  :hook ((go-ts-mode . eglot-ensure))
  ;; :config
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  )

(use-package just-ts-mode
  ;; :mode ("justfile\\'" . just-ts-mode)
  :hook ((just-ts-mode . eglot-ensure))
  )
;; (with-eval-after-load 'eglot
;;   (add-to-list 'eglot-server-programs
;;                '(just-ts-mode . ("just-lsp" "--stdio"))))

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" . typescript-ts-mode)
  :mode ("\\.js\\'" . typescript-ts-mode)
  :hook ((typescript-ts-mode . eglot-ensure))
  ;; :config
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  )
(use-package tsx-ts-mode
  :ensure nil
  :mode ("\\.tsx\\'" . tsx-ts-mode)
  :hook ((tsx-ts-mode . eglot-ensure))
  ;; :config
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  )

(use-package flycheck
  :hook (lsp-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-previous-error)
              ("M-p" . flycheck-next-error))
  :custom (flycheck-display-errors-delay .3))

(use-package dape
  ;; :preface
  ;; ;; By default dape shares the same keybinding prefix as `gud'
  ;; ;; If you do not want to use any prefix, set it to nil.
  ;; ;; (setq dape-key-prefix "\C-x\C-a")

  ;; :hook
  ;; ;; Save breakpoints on quit
  ;; ;; (kill-emacs . dape-breakpoint-save)
  ;; ;; Load breakpoints on startup
  ;; ;; (after-init . dape-breakpoint-load)

  ;; :config
  ;; ;; Turn on global bindings for setting breakpoints with mouse
  ;; ;; (dape-breakpoint-global-mode)

  ;; ;; Info buffers to the right
  ;; ;; (setq dape-buffer-window-arrangement 'right)

  ;; ;; Info buffers like gud (gdb-mi)
  ;; ;; (setq dape-buffer-window-arrangement 'gud)
  ;; ;; (setq dape-info-hide-mode-line nil)

  ;; ;; Pulse source line (performance hit)
  ;; ;; (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

  ;; ;; Showing inlay hints
  ;; ;; (setq dape-inlay-hints t)

  ;; ;; Save buffers on startup, useful for interpreted languages
  ;; ;; (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))

  ;; ;; Kill compile buffer on build success
  ;; ;; (add-hook 'dape-compile-hook 'kill-buffer)

  ;; ;; Projectile users
  ;; ;; (setq dape-cwd-function 'projectile-project-root)
  )

;; Enable repeat mode for more ergonomic `dape' use
(use-package repeat
  :ensure nil
  :config
  (repeat-mode))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . eglot-ensure))
  ;; :config
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  )
(dolist (mode '((nix-mode . ("nixd"))))
  (add-to-list 'eglot-server-programs mode))

(use-package python-ts-mode
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :hook ((python-ts-mode . eglot-ensure))
  ;; :config
  ;; (add-hook 'go-ts-mode-hook 'eglot-ensure)
  )

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-cargo-use-last-stored-arguments t))

(load (locate-user-emacs-file "lisp/kcl-mode.el"))
(use-package kcl-ts-mode
  :ensure nil
  :mode "\\.k\\'"
  :hook ((kcl-ts-mode . eglot-ensure))
  )

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
	       '(kcl-ts-mode . ("kcl-language-server")))
  )

(use-package yaml-pro
  ;; :hook
  ;; (yaml-ts-mode . #'yaml-pro mode 100)
  ;; :mode ("\\.yaml\\'" . yaml-ts-mode)
  ;; :mode ("\\.yml\\'" . yaml-ts-mode)
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook (
	 (yaml-ts-mode . yaml-pro-ts-mode)
	 (yaml-ts-mode . eglot-ensure)
	 )
  )
;; (use-package yaml-mode
;;   :ensure t
;;   :mode ("\\.ya?ml\\'" . yaml-mode)
;;   :hook (yaml-mode . eglot-ensure))

(use-package eglot
  :ensure nil  ; Built-in to Emacs 29+
  :config
  (add-to-list 'eglot-server-programs
               '(yaml-mode . ("yaml-language-server" "--stdio")))
  
  ;; Configure YAML language server settings
  (setq-default eglot-workspace-configuration
                '((:yaml . ((:kubernetes . "*.yaml")
                           (:schemas . ((:kubernetes . "*.yaml")
                                      ("https://raw.githubusercontent.com/yannh/kubernetes-json-schema/master/v1.28.0/all.json" . "*.k8s.yaml")))
                           (:completion . t)
                           (:hover . t)
                           (:validate . t))))))

(use-package aider
  :config
  (setq aider-args '("--model" "ollama_chat/qwen3:1.7b" ))
  ;; (setq aider-args '("--model" "ollama_chat/qwen3:4b" ))
  ;; (setq aider-args '("--model" "ollama_chat/qwen3:8b" ))
  (setenv "OLLAMA_API_BASE" "http://127.0.0.1:11434")
  (global-set-key (kbd "C-c a") 'aider-transient-menu)) ;; for wider screen

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))
