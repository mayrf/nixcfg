;; ;; Inspired by https://github.com/nix-community/nix-ts-mode/blob/trunk/nix-ts-mode.el

;; ;;; kcl-ts-mode.el --- Major mode for Nix expressions, powered by tree-sitter -*- lexical-binding: t -*-

;; ;; Maintainer: Remi Gelinas <mail@remigelin.as>
;; ;; Homepage: https://github.com/nix-community/kcl-ts-mode
;; ;; Version: 0.1.4
;; ;; Keywords: nix languages
;; ;; Package-Requires: ((emacs "29.1"))

;; ;; This program is free software; you can redistribute it and/or modify
;; ;; it under the terms of the GNU General Public License as published by
;; ;; the Free Software Foundation, either version 3 of the License, or
;; ;; (at your option) any later version.

;; ;; This program is distributed in the hope that it will be useful,
;; ;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; ;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; ;; GNU General Public License for more details.

;; ;; You should have received a copy of the GNU General Public License
;; ;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; ;; This file is NOT part of GNU Emacs.

;; ;;; Commentary:

;; ;; A major mode for editing Kcl expressions, powered by the new
;; ;; built-in tree-sitter support in Emacs 29.1.

;; ;;; Code:
;;  ;; (unless (version< emacs-version "29.1")
;;  ;;   (error "`kcl-ts-mode` requires at least Emacs 29 for tree-sitter support"))

;; (require 'treesit)

;; (unless (treesit-available-p)
;;   (error "`kcl-ts-mode` requires Emacs to be built with tree-sitter support"))

;; ;; (declare-function treesit-parser-create "treesit.c")
;; ;; (declare-function treesit-node-child-by-field-name "treesit.c")
;; ;; (declare-function treesit-node-type "treesit.c")

;; ;; Other

;; (defgroup kcl-ts nil
;;   "Major mode for editing Nix expressions."
;;   :prefix "kcl-ts-"
;;   :group 'languages)

;; (defcustom kcl-ts-mode-indent-offset 2
;;   "Number of spaces for each indentation step in `kcl-ts-mode'."
;;   :type 'integer
;;   :safe 'integerp)

;; (defcustom kcl-ts-mode-indent-level 4
;;   "Number of spaces for each indentation step in `kcl-ts-mode'."
;;   :type 'integer
;;   :safe 'integerp)

;; (defvar kcl-ts--treesit-builtins
;;   ;; nix eval --impure --expr 'with builtins; filter (x: !(elem x [ "abort" "derivation" "import" "throw" ]) && isFunction builtins.${x}) (attrNames builtins)'
;;   '("add" "addErrorContext" "all" "any" "appendContext" "attrNames" "attrValues" "baseNameOf" "bitAnd" "bitOr" "bitXor" "break" "catAttrs" "ceil" "compareVersions" "concatLists" "concatMap" "concatStringsSep" "deepSeq" "derivationStrict" "dirOf" "div" "elem" "elemAt" "fetchGit" "fetchMercurial" "fetchTarball" "fetchTree" "fetchurl" "filter" "filterSource" "findFile" "floor" "foldl'" "fromJSON" "fromTOML" "functionArgs" "genList" "genericClosure" "getAttr" "getContext" "getEnv" "getFlake" "groupBy" "hasAttr" "hasContext" "hashFile" "hashString" "head" "intersectAttrs" "isAttrs" "isBool" "isFloat" "isFunction" "isInt" "isList" "isNull" "isPath" "isString" "length" "lessThan" "listToAttrs" "map" "mapAttrs" "match" "mul" "parseDrvName" "partition" "path" "pathExists" "placeholder" "readDir" "readFile" "removeAttrs" "replaceStrings" "scopedImport" "seq" "sort" "split" "splitVersion" "storePath" "stringLength" "sub" "substring" "tail" "toFile" "toJSON" "toPath" "toString" "toXML" "trace" "traceVerbose" "tryEval" "typeOf" "unsafeDiscardOutputDependency" "unsafeDiscardStringContext" "unsafeGetAttrPos" "zipAttrsWith"))

;; (defvar kcl-ts--treesit-constants
;;   ;; nix eval --impure --expr 'with builtins; filter (x: !(isFunction builtins.${x} || isBool builtins.${x})) (attrNames builtins)'
;;   '("builtins" "currentSystem" "currentTime" "langVersion" "nixPath" "nixVersion" "null" "storeDir"))

;; ;; Settings
;; (defvar kcl-ts-mode--font-lock-settings
;;   (treesit-font-lock-rules
;;    :language 'kcl
;;    :feature 'bracket
;;    '(["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)

;;    :language 'kcl
;;    :feature 'comment
;;    '((comment) @font-lock-comment-face)

;;    :language 'nix
;;    :feature 'delimiter
;;    '(["," "." ";"] @font-lock-delimiter-face)

;;    :language 'nix
;;    :feature 'keyword
;;    `((let_expression
;;       (["let" "in"] @font-lock-keyword-face))
;;      (if_expression
;;       ["if" "then" "else"] @font-lock-keyword-face)
;;      (rec_attrset_expression
;;       ("rec" @font-lock-keyword-face))
;;      (with_expression
;;       ("with" @font-lock-keyword-face))
;;      (inherit
;;       ("inherit" @font-lock-keyword-face))
;;      (inherit_from
;;       ("inherit" @font-lock-keyword-face))
;;      (assert_expression
;;       ("assert" @font-lock-keyword-face))
;;      ((identifier) @font-lock-keyword-face
;;       (:match
;;        ,(rx-to-string
;;          `(seq bol (or "throw" "abort")
;;                eol))
;;        @font-lock-keyword-face))

;;      ;; "or" is technically an operator, but we fontify it as a keyword
;;      (select_expression
;;       ("or" @font-lock-keyword-face)))

;;    :language 'nix
;;    :feature 'string
;;    :override t
;;    `((string_fragment) @font-lock-string-face
;;      (string_expression
;;       ("\"" @font-lock-string-face))
;;      (indented_string_expression
;;       ("''" @font-lock-string-face))
;;      (interpolation
;;       (["${" "}"] @font-lock-misc-punctuation-face)))

;;    :language 'nix
;;    :feature 'operator
;;    `((binary_expression operator: _ @font-lock-operator-face)
;;      (unary_expression operator: _ @font-lock-operator-face))

;;    :language 'nix
;;    :feature 'number
;;    `([(integer_expression) (float_expression)] @font-lock-constant-face)

;;    :language 'nix
;;    :feature 'path
;;    `((path_expression
;;       (path_fragment) @font-lock-string-face))

;;    :language 'nix
;;    :feature 'builtin
;;    `((variable_expression name: (identifier) @font-lock-builtin-face
;;                           (:match
;;                            ,(rx-to-string
;;                              `(seq bol (or ,@kcl-ts--treesit-builtins)
;;                                    eol))
;;                            @font-lock-builtin-face)))
;;    :language 'nix
;;    :feature 'constant
;;    `((variable_expression name: (identifier) @font-lock-constant-face
;;                           (:match
;;                            ,(rx-to-string
;;                              `(seq bol (or ,@kcl-ts--treesit-constants "true" "false")
;;                                    eol))
;;                            @font-lock-constant-face)))
;;    :language 'nix
;;    :feature 'attribute
;;    `((attrpath
;;       (identifier) @font-lock-variable-name-face))

;;    :language 'nix
;;    :feature 'ellipses
;;    `((ellipses) @font-lock-misc-punctuation-face)

;;    :language 'nix
;;    :feature 'function
;;    `((function_expression
;;       ":" @font-lock-misc-punctuation-face))

;;    :language 'nix
;;    :feature 'error
;;    :override t
;;    '((ERROR) @font-lock-warning-face))
;;   "Tree-sitter font-lock settings for `kcl-ts-mode'.")

;; ;; Indentation
;; (defun kcl-ts-indent-multiline-string (n parent bol &rest rest)
;;   "Return the indent prefix for the current multi-line string line.
;; For the first line, this is the previous line offset+nix-indent-offset,
;; and for subsequent lines it's the previous line's indentation."
;;   ;; If the current line is the first relevant one in the multiline
;;   ;; string, indent it to the default level (2 spaces past the
;;   ;; previous line's offset):
;;   (if (and (equal (treesit-node-child (treesit-node-parent parent) 1)
;;                   parent)
;;            (<= (count-lines (treesit-node-start parent) (point)) 1))
;;       (+ (apply (alist-get 'parent-bol treesit-simple-indent-presets)
;;                 n parent bol rest)
;;          kcl-ts-mode-indent-offset)
;;     ;; If the current line is already indented to some level, leave it alone:
;;     (if (/= bol
;;             (save-excursion
;;               (beginning-of-line)
;;               (point)))
;;         bol
;;       ;; otherwise, indent to the level of the previous line by default.
;;       (save-excursion
;;         (forward-line -1)
;;         (if (looking-at "\s+")
;;             (match-end 0)
;;           ;; in case neither line has discernable indentation, just
;;           ;; indent to bol
;;           bol)))))

;; (defvar kcl-ts-mode-indent-rules
;;   `((kcl
;;      ((parent-is "source_code") column-0 0)
;;      ((node-is "]") parent-bol 0)
;;      ((node-is ")") parent-bol 0)
;;      ((node-is "}") parent-bol 0)
;;      ((node-is "then") parent-bol 0)
;;      ((node-is "else") parent-bol 0)
;;      ((node-is ";") parent-bol 0)
;;      ((node-is "^in$") parent-bol 0)
;;      ((node-is "binding_set") parent-bol kcl-ts-mode-indent-offset)
;;      ((match "interpolation" "indented_string_expression" nil nil nil) kcl-ts-indent-multiline-string 0)
;;      ((parent-is "indented_string_expression") parent-bol 0)
;;      ((parent-is "string_fragment") kcl-ts-indent-multiline-string 0)
;;      ((parent-is "binding_set") parent-bol 0)
;;      ((parent-is "binding") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "let_expression") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "attrset_expression") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "list_expression") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "apply_expression") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "parenthesized_expression") parent-bol kcl-ts-mode-indent-offset)
;;      ((parent-is "formals") parent-bol kcl-ts-mode-indent-offset)))
;;   "Tree-sitter indent rules for `kcl-ts-mode'.")

;; ;; Keymap
;; (defvar kcl-ts-mode-map
;;   (let ((map (make-sparse-keymap)))
;;     map)
;;   "Keymap for `kcl-ts-mode'.")

;; ;; Syntax map
;; (defvar kcl-ts-mode--syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?# "< b" table)
;;     (modify-syntax-entry ?\n "> b" table)
;;     (modify-syntax-entry ?/ ". 14" table)
;;     (modify-syntax-entry ?* ". 23" table)
;;     table)
;;   "Syntax table for `kcl-ts-mode'.")

;; (defun kcl-ts-mode--defun-name (node)
;;   "Return the defun name of NODE.
;; Return nil if there is no name or if NODE is not a defun node."
;;   (pcase (treesit-node-type node)
;;     ("binding"
;;      (treesit-node-text
;;       (treesit-node-child-by-field-name node "attrpath") t))))

;; (define-derived-mode kcl-ts-mode prog-mode "kcl"
;;   "Major mode for editing Nix expressions, powered by treesitter.

;; ;; \\{kcl-ts-mode-map}"
;; ;;   :syntax-table kcl-ts-mode--syntax-table

;;    (when (treesit-ready-p 'kcl)
;;      (treesit-parser-create 'kcl)

;; ;;     ;; Font locking
;; ;;     (setq-local treesit-font-lock-settings kcl-ts-mode--font-lock-settings)

;; ;;     (setq-local treesit-font-lock-feature-list
;; ;;                 '((comment builtin)
;; ;;                   (keyword string path)
;; ;;                   (number constant attribute)
;; ;;                   (bracket delimiter error operator ellipses function)))

;; ;;     ;; Comments
;;      (setq-local comment-start "# ")
;;      (setq-local comment-start "# ")
;;      (setq-local comment-start-skip "#+\\s-*")

;; ;;     ;; Indentation
;; ;;     (setq-local treesit-simple-indent-rules kcl-ts-mode-indent-rules)

;; ;;     ;; Imenu.
;; ;;     (setq-local treesit-simple-imenu-settings
;; ;;                 `((nil "\\`binding\\'" nil nil)))

;; ;;     ;; Navigation.
;; ;;     (setq-local treesit-defun-type-regexp (rx (or "binding")))
;; ;;     (setq-local treesit-defun-name-function #'kcl-ts-mode--defun-name)

;;      (treesit-major-mode-setup))

;; ;;   (when (functionp 'derived-mode-add-parents)
;; ;;     (derived-mode-add-parents 'kcl-ts-mode '(nix-mode))))
;;    )
(defvar kcl-ts-font-lock-rules
  '(
    ;; :language html
    ;;   :override t
    ;;   :feature delimiter
    ;;   (["<" ">" "/>" "</"] @font-lock-bracket-face)

      :language kcl
      :override t
      :feature comment
      ((comment) @font-lock-comment-face)

      :language kcl
      :feature bracket
      ;; (["(" ")" "[" "]" "{" "}"] @font-lock-bracket-face)
      (["(" ")" "[" "]" "{" "}"] @font-lock-constant-face)

      :language kcl
      :override t
      :feature call_expr
      ((call_expr) @font-lock-function-call-face)

      ))


;; font-lock-function-call-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-function-name-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-bracket-face                        abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-builtin-face                        abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-comment-delimiter-face              abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-comment-face                        abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-constant-face                       abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-delimiter-face                      abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-doc-face                            abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-doc-markup-face                     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-escape-face                         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-function-call-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-function-name-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-keyword-face                        abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-misc-punctuation-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-negation-char-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-number-face                         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-operator-face                       abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-preprocessor-face                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-property-name-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-property-use-face                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-punctuation-face                    abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-regexp-face                         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-regexp-grouping-backslash           abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-regexp-grouping-construct           abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-string-face                         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-type-face                           abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-variable-name-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-variable-use-face                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-warning-face                        abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
  (defun kcl-ts-setup ()
    "Setup treesit for kcl-ts-mode."
    ;; Our tree-sitter setup goes here.

    ;; This handles font locking -- more on that below.
    (setq-local treesit-font-lock-settings
                 (apply #'treesit-font-lock-rules
                      kcl-ts-font-lock-rules))

    ;; This handles indentation -- again, more on that below.
    (setq-local treesit-font-lock-feature-list
                '((comment)
                  (call_expr)
                  (assign_stmt)
                  (bracket)
                  (constant tag attribute)
                  (declaration)
                  (delimiter)))
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

    (defvar kcl-ts-mode-indent-rules
      `((kcl
         ((parent-is "assign_stmt") parent kcl-ts-mode-indent-offset)
         ;; ((parent-is "source_code") column-0 0)
         ;; ((node-is "]") parent-bol 0)
         ;; ((node-is ")") parent-bol 0)
         ;; ((node-is "}") parent-bol 0)
         ;; ((node-is "then") parent-bol 0)
         ;; ((node-is "else") parent-bol 0)
         ;; ((node-is ";") parent-bol 0)
         ;; ((node-is "^in$") parent-bol 0)
         ;; ((node-is "binding_set") parent-bol kcl-ts-mode-indent-offset)
         ;; ((match "interpolation" "indented_string_expression" nil nil nil) kcl-ts-indent-multiline-string 0)
         ;; ((parent-is "indented_string_expression") parent-bol 0)
         ;; ((parent-is "string_fragment") kcl-ts-indent-multiline-string 0)
         ;; ((parent-is "binding_set") parent-bol 0)
         ;; ((parent-is "binding") parent-bol kcl-ts-mode-indent-offset)
         ;; ((parent-is "let_expression") parent-bol kcl-ts-mode-indent-offset)
         ;; ((parent-is "attrset_expression") parent-bol kcl-ts-mode-indent-offset)
         ;; ((parent-is "list_expression") parent-bol kcl-ts-mode-indent-offset)
         ;; ((parent-is "apply_expression") parent-bol kcl-ts-mode-indent-offset)
         ;; ((parent-is "parenthesized_expression") parent-bol kcl-ts-mode-indent-offset)
         ((parent-is "formals") parent-bol kcl-ts-mode-indent-offset)))
      "Tree-sitter indent rules for `kcl-ts-mode'.")



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

(provide 'kcl-ts-mode)
;; ;;; kcl-ts-mode.el ends here

;; ;; Local Variables:
;; ;; indent-tabs-mode: nil
;; ;; End:
