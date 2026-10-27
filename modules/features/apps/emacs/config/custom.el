;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-anthropic-default-model-id "sonnet")
 '(agent-shell-inhibit-system-sleep nil)
 '(agent-shell-preferred-agent-config 'claude-code)
 '(agent-shell-session-restore-verbosity 'full)
 '(agent-shell-tool-use-expand-by-default t)
 '(agent-shell-user-message-expand-by-default t)
 '(auto-save-visited-mode t)
 '(dired-preview-delay 0.0)
 '(format-all-default-formatters
   '(("Assembly" asmfmt) ("ATS" atsfmt) ("Bazel" buildifier)
     ("BibTeX" emacs-bibtex) ("C" clang-format) ("C#" csharpier)
     ("" nil) ("C++" clang-format) ("Cabal Config" cabal-fmt)
     ("Clojure" zprint) ("CMake" cmake-format) ("Crystal" crystal)
     ("CSS" prettier) ("Cuda" clang-format) ("D" dfmt)
     ("Dart" dart-format) ("Dhall" dhall) ("Dockerfile" dockfmt)
     ("Elixir" mix-format) ("Elm" elm-format)
     ("Emacs Lisp" emacs-lisp) ("Erlang" efmt) ("F#" fantomas)
     ("Fish" fish-indent) ("Fortran Free Form" fprettify)
     ("GLSL" clang-format) ("Go" gofmt) ("GraphQL" prettier)
     ("GDScript" gdscript-formatter) ("Haskell" brittany)
     ("HCL" hclfmt) ("HLSL" clang-format) ("HTML" html-tidy)
     ("HTML+EEX" mix-format) ("HTML+ERB" erb-format) ("Hy" emacs-hy)
     ("Java" google-java-format) ("JavaScript" prettier)
     ("JSON" prettier) ("JSON5" prettier) ("Jsonnet" jsonnetfmt)
     ("JSX" prettier) ("Kotlin" ktlint) ("LaTeX" latexindent)
     ("Less" prettier) ("Literate Haskell" brittany) ("Lua" lua-fmt)
     ("Markdown" prettier) ("Meson" muon-fmt) ("Nix" alejandra)
     ("Objective-C" clang-format) ("OCaml" ocp-indent)
     ("Perl" perltidy) ("PHP" prettier) q
     ("Protocol Buffer" clang-format) ("PureScript" purty)
     ("Python" black) ("R" styler) ("Reason" bsrefmt)
     ("ReScript" rescript) ("Ruby" rufo) ("Rust" rustfmt)
     ("Scala" scalafmt) ("SCSS" prettier) ("Shell" shfmt)
     ("Solidity" prettier) ("SQL" sqlformat) ("Svelte" prettier)
     ("Swift" swiftformat) ("Terraform" terraform-fmt)
     ("TOML" prettier) ("TSX" prettier) ("TypeScript" prettier)
     ("Typst" typstyle) ("V" v-fmt) ("Verilog" istyle-verilog)
     ("Vue" prettier) ("XML" html-tidy) ("YAML" prettier) ("Zig" zig)
     ("_Angular" prettier) ("_AZSL" clang-format)
     ("_Beancount" bean-format) ("_Caddyfile" caddy-fmt)
     ("_Flow" prettier) ("_Gleam" gleam) ("_Ledger" ledger-mode)
     ("_Nginx" nginxfmt) ("_Snakemake" snakefmt)))
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info
             ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-selected-packages
   '(agent-shell agent-shell-hq avy cape corfu denote-roam direnv dirvish
                 eat ef-themes elisp-demos ellama embark
                 embark-consult evil forge format-all helpful jinx
                 just-mode justl knockknock lab load-env-vars magit
                 marginalia nerd-icons nix-ts-mode no-littering
                 olivetti orderless org-cliplink org-jira org-noter
                 org-rich-yank org-roam org-roam-ui org-super-agenda
                 pdf-tools popper popper-mode posframe prescient
                 vertico vterm vterm-toggle web-mode yasnippet
                 yasnippet-snippets))
 '(package-vc-selected-packages
   '((agent-shell-desktop :url
                          "https://github.com/timfel/agent-shell-desktop.el")
     (agent-shell-hq :url
                     "https://github.com/sreenivasvrao/agent-shell-hq")
     (knockknock :url "https://github.com/konrad1977/knockknock.git")
     (denote-roam :url "https://github.com/BardofSprites/denote-roam"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
