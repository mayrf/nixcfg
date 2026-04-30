;; -*- lexical-binding: t; -*-

;; ;; Inspired by https://github.com/nix-community/nix-ts-mode/blob/trunk/nix-ts-mode.el

;; ;;; kcl-ts-mode.el --- Major mode for Nix expressions, powered by tree-sitter -*- lexical-binding: t -*-

(setq kcl-ts-font-lock-rules
  '(

      :language kcl
      :override t
      :feature comment
      ((comment) @font-lock-comment-face)

      :language kcl
      :override t
      :feature bracket
      (["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

     :language kcl 
     :feature call_expr
     ((config_entry
         [(identifier) (selector_expr)] @font-lock-property-name-face))

     :language kcl 
     :feature function
     ((call_expr
        function:
         (identifier) @font-lock-function-call-face))

     :language kcl 
     :feature function
     ((call_expr
        function:
	(selector_expr
	 (identifier) @font-lock-variable-use-face
	 (select_suffix) @font-lock-function-call-face)))
        ;; (identifier) @font-lock-string-face))
;; @font-lock-string-face
         ;; (member_expression
         ;;  property: (property_identifier) @font-lock-function-call-face)]))

     :language kcl
     :feature string
     ((string) @font-lock-string-face)

      ))


  (defun kcl-ts-setup ()
    "Setup treesit for kcl-ts-mode."
    ;; Our tree-sitter setup goes here.

    ;; This handles font locking -- more on that below.
    (setq-local treesit-font-lock-settings
                 (apply #'treesit-font-lock-rules
                      kcl-ts-font-lock-rules))

    ;; This handles indentation -- again, more on that below.
    (setq-local treesit-font-lock-feature-list
                '((comment bracket constant tag attribute)
                  (call_expr function declaration delimiter)
                  (assign_stmt keyword string path)))
    ;; ... everything else we talk about go here also ...
    (defcustom kcl-ts-mode-indent-offset 4
      "Number of spaces for each indentation step in `kcl-ts-mode'."
      :type 'integer
      :safe 'integerp)

    (defcustom kcl-ts-mode-indent-level 4
      "Number of spaces for each indentation step in `kcl-ts-mode'."
      :type 'integer
      :safe 'integerp)
    (setq treesit--indent-verbose t)

    ;; (defvar kcl-ts-mode-indent-rules
    (setq kcl-ts-mode-indent-rules
      `((kcl
         ((node-is "}") parent-bol 0)
         ((node-is "]") parent-bol 0)
         ((node-is ")") parent-bol 0)
         ;; ((and (no-node) (parent-is ")") parent-bol 0)
         ;; ((and no-node (parent-is "{")) parent-bol kcl-ts-mode-indent-offset)
         ;; ((and (no-node) (parent-is "}") parent-bol 0)
         ;; ((and (no-node) (parent-is "]") parent-bol 0)
         ;; ((and (no-node) (prev-sibling "config_entry")) parent-bol 0)
         ((parent-is "assign_stmt") parent-bol kcl-ts-mode-indent-offset)
         ((parent-is "{") parent-bol kcl-ts-mode-indent-offset)
         ((parent-is "config_expr") parent-bol kcl-ts-mode-indent-offset)
         ((parent-is "if_stmt") parent-bol kcl-ts-mode-indent-offset)
         ((parent-is "list_expr") parent-bol kcl-ts-mode-indent-offset)
         ((node-is "config_entry") first-sibling 0)
         ((parent-is "formals") parent-bol kcl-ts-mode-indent-offset)
         (no-node parent-bol 0)
	 )
	)
      )
      ;; "Tree-sitter indent rules for `kcl-ts-mode'.")



    (setq-local treesit-simple-indent-rules kcl-ts-mode-indent-rules)
    ;; End with this
    (treesit-major-mode-setup))

;;;###autoload
(define-derived-mode kcl-ts-mode prog-mode "kcl"
  "Major mode for editing kcl-lang files with tree-sitter."
  ;; :syntax-table kcl-ts-mode--syntax-table

  ;; Comments
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*")

  (setq-local font-lock-defaults nil)
  (when (treesit-ready-p 'kcl)
    (treesit-parser-create 'kcl)
    (kcl-ts-setup)))


;; ;; Local Variables:
;; ;; indent-tabs-mode: nil
;; ;; End:

(defun kcl-reload ()
  (interactive)
  (load-file "~/.config/nixcfg/home/features/editor/emacs/vanilla/lisp/kcl-mode.el")
  (kcl-ts-mode)
  )

(defun process-region-with-command (command)
  "Process the currently marked region with a CLI command and display output in a popup buffer.
   If no region is selected, process the entire buffer.
   COMMAND can include pipes, redirection, and other shell features."
  (interactive "sCommand: ")
  (let* ((use-whole-buffer (not (use-region-p)))
         (region-beginning (if use-whole-buffer (point-min) (region-beginning)))
         (region-end (if use-whole-buffer (point-max) (region-end)))
         (region-text (buffer-substring-no-properties region-beginning region-end))
         (buffer-name (format "*%s Output*" command))
         (buffer (get-buffer-create buffer-name)))

    ;; Clear the buffer if it already exists
    (with-current-buffer buffer
      (erase-buffer))
    ;; Create a temporary file for the input
    (let ((temp-file (make-temp-file "emacs-region-")))
      (unwind-protect
          (progn
            ;; Write region/buffer content to temp file
            (with-temp-file temp-file
              (insert region-text))
            
            ;; Execute command and capture output directly to buffer
            (with-current-buffer buffer
              (call-process-shell-command 
               (format "cat %s | %s" 
                       (shell-quote-argument temp-file) 
                       command)
               nil t)))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))
    
    ;; Display the buffer
    (display-buffer buffer '(display-buffer-pop-up-window . nil))
    
    ;; Provide feedback about what was processed
    (message (if use-whole-buffer 
                 "Processed entire buffer with '%s'" 
               "Processed region with '%s'")
             command)))
(defun process-region-with-command-replace (command)
  "Process the currently marked region with a CLI command and replace with the output.
   If no region is selected, process the entire buffer."
  (interactive)
  (let* (
         ;; (command (read-string "Command: "))
         (use-whole-buffer (not (use-region-p)))
         (region-beginning (if use-whole-buffer (point-min) (region-beginning)))
         (region-end (if use-whole-buffer (point-max) (region-end)))
         (region-text (buffer-substring-no-properties region-beginning region-end))
         (output nil))
    
    ;; Create a temporary file for the input
    (let ((temp-file (make-temp-file "emacs-region-")))
      (unwind-protect
          (progn
            ;; Write region/buffer content to temp file
            (with-temp-file temp-file
              (insert region-text))
            
            ;; Execute command and capture output
            (setq output
                  (with-temp-buffer
                    (call-process-shell-command 
                     (format "cat %s | %s" 
                             (shell-quote-argument temp-file) 
                             command)
                     nil t)
                    (buffer-string))))
        ;; Clean up temp file
        (when (file-exists-p temp-file)
          (delete-file temp-file))))
    
    ;; Replace the region with the output
    (delete-region region-beginning region-end)
    (goto-char region-beginning)
    (insert output)
    
    ;; Provide feedback
    (message (if use-whole-buffer 
                 "Processed and replaced entire buffer with '%s'" 
               "Processed and replaced region with '%s'")
             command)))

(setq kcl-import-yaml-command
            "cat > temp_input
             kcl import -m yaml temp_input --output temp_output --force
             cat temp_output | tail -n +6
             rm temp_input temp_output"
            )

(defun region-to-kcl ()
  "Run the selected region through jq and display the output in a popup buffer."
  (interactive)
  (process-region-with-command kcl-import-yaml-command))


(defun region-to-kcl-replace ()
  "Run the selected region through jq and display the output in a popup buffer."
  (interactive)
  (process-region-with-command-replace kcl-import-yaml-command))


(provide 'kcl-ts-mode)
;; ;;; kcl-ts-mode.el ends here
