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

(provide 'mayrf-emacs-keybindings)
