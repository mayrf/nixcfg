; -*- lexical-binding: t; -*-
;; ---------------------------------------------------------------------
;; agent-shell + agent-shell-desktop: persist ONLY agent-shell sessions
;; ---------------------------------------------------------------------

;; 1. Load both packages eagerly — no autoload/deferred loading, so
;;    agent-shell-desktop-mode is guaranteed active before we ever call
;;    desktop-read below. This avoids the "empty invisible buffer" bug
;;    caused by desktop-read running before the mode handler is registered.
(use-package agent-shell
  :demand t
  :config
  ;; run
  ;; npx mcp-remote https://gitlab.mcp.ai.vienna.hobex.io/mcp --enable-proxy
  ;; npx mcp-remote https://atlassian.mcp.ai.vienna.hobex.io/mcp --enable-proxy
  ;; to register
  (setq agent-shell-mcp-servers
        '(((name . "atlassian")
           (type . "stdio")
           (command . "npx")
           (args . ["-y" "mcp-remote" "https://ai.vienna.hobex.io/atlassian_mcp/mcp" "--enable-proxy"]))
          ((name . "gitlab")
           (type . "stdio")
           (command . "npx")
           (args . ["-y" "mcp-remote" "https://ai.vienna.hobex.io/gitlab_mcp/mcp" "--enable-proxy"]))))
  (defun my/agent-shell-git-main-root ()
    "Root of the main worktree, even when called from inside a linked worktree."
    (let ((common-dir (string-trim
                       (shell-command-to-string
                        "git rev-parse --path-format=absolute --git-common-dir 2>/dev/null"))))
      (unless (string-empty-p common-dir)
        (file-name-directory (directory-file-name common-dir)))))

  (advice-add 'agent-shell-new-worktree-shell :around
              (lambda (orig-fn &rest args)
                (let ((default-directory (or (my/agent-shell-git-main-root)
                                             default-directory)))
                  (apply orig-fn args))))

  )

(use-package agent-shell-hq
  :vc (:url "https://github.com/sreenivasvrao/agent-shell-hq" :rev :newest)
  :after (agent-shell posframe)
  :commands (agent-shell-hq-toggle agent-shell-hq-peek agent-shell-hq-label))

;; (use-package agent-shell-desktop
;;   :vc (:url "https://github.com/timfel/agent-shell-desktop.el")
;;   :demand t
;;   :after agent-shell
;;   :config
;;   (agent-shell-desktop-mode 1))

;; ;; 2. Replay full conversation history on resume instead of just the title.
;; (setq agent-shell-session-restore-verbosity 'full)  ; or 'last for lighter-weight

;; ;; 3. Scope desktop.el down so it only ever saves/restores agent-shell
;; ;;    buffers — no window layout, no file-visiting buffers, nothing else.
;; (setq desktop-restore-frames nil
;;       desktop-files-not-to-save "\\`.*\\'"   ; don't try to restore file buffers
;;       desktop-buffers-not-to-save "\\`.*\\'" ; belt-and-suspenders, see advice below
;;       desktop-restore-eager t                ; restore all matching buffers immediately
;;       desktop-load-locked-desktop t
;;       desktop-save-mode nil)                 ; we'll drive save/restore manually (see below)

;; (advice-add 'desktop-save-buffer-p :filter-return
;;             (lambda (result)
;;               "Only ever let desktop.el save/restore agent-shell-mode buffers."
;;               (and result (derived-mode-p 'agent-shell-mode))))

;; ;; 4. Autosave the (agent-shell-only) desktop periodically and on exit,
;; ;;    since we're not using desktop-save-mode's own autosave.
;; (defvar my/agent-shell-desktop-dirname user-emacs-directory
;;   "Where to store the agent-shell desktop file.")

;; (defun my/agent-shell-desktop-save ()
;;   "Save only agent-shell buffers via desktop.el."
;;   (when (agent-shell-buffers)
;;     (let ((desktop-dirname my/agent-shell-desktop-dirname))
;;       (desktop-save desktop-dirname t))))

;; (add-hook 'kill-emacs-hook #'my/agent-shell-desktop-save)
;; (run-with-idle-timer 60 t #'my/agent-shell-desktop-save)

;; ;; 5. Restore explicitly, once we know agent-shell-desktop-mode is on
;; ;;    (rather than relying on desktop-save-mode's own startup hook timing).
;; (defun my/agent-shell-desktop-restore ()
;;   "Restore saved agent-shell sessions."
;;   (interactive)
;;   (unless agent-shell-desktop-mode
;;     (user-error "agent-shell-desktop-mode is not enabled"))
;;   (let ((desktop-dirname my/agent-shell-desktop-dirname))
;;     (when (file-exists-p (desktop-full-file-name desktop-dirname))
;;       (desktop-read desktop-dirname))))

;; ;; Run after init, once all packages above are fully loaded.
;; (add-hook 'emacs-startup-hook #'my/agent-shell-desktop-restore)

(provide 'agent-shell-setup)
;;; agent-shell-setup.el ends here
