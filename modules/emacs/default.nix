{
  inputs,
  self,
  lib,
  pkgs,
  ...
}:
let
  client-or-server = ''emacsclient --socket-name=emacs --create-frame --alternate-editor="emacs --init-directory=~/.config/nixcfg/modules/emacs/config" "$@"'';
  
in
{
  flake.modules.nixos.emacs =
    {
      lib,
      config,
      ...
    }:
    {
      # emacs HM configuration is applied per-host via flake.modules.homeManager.emacs
    };

  flake.modules.homeManager.emacs-vanilla =
    {
      config,
      osConfig,
      pkgs,
      lib,
      private,
      host,
      ...
    }:
    let
      emacs = (
        (pkgs.emacsPackagesFor pkgs.emacs-git).emacsWithPackages (epkgs: [
          epkgs.vterm
          epkgs.emacsql
          epkgs.pdf-tools
          epkgs.org
          epkgs.treesit-grammars.with-all-grammars
          epkgs.jinx
        ])
      );
    in
    {
      home.packages = with pkgs; [
	"${emacs}"
	(pkgs.writeShellScriptBin "vanemacs" ''
          ${client-or-server}
	'')
	

	claude-agent-acp # agent-shell
	
	mupdf # doc-view-mode
	mpv # elfeed yt
	yt-dlp-light
	clang-tools # clangd (c-lsp)
      ];
    };
  flake.modules.homeManager.emacs =
    {
      config,
      osConfig,
      pkgs,
      lib,
      private,
      host,
      ...
    }:
    {
      features.impermanence.directories = [
        ".config/emacs-doom"
        ".config/dotemacs"
      ];

  
      home.file."${config.xdg.configHome}/enchant".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Documents/org/shared/.config/enchant";

  
          
  
      home.packages = with pkgs; [
        # vanilla fonts:
        dejavu_fonts
        liberation_ttf

        # Doom emacs dependencies
        lldb
        git
        (ripgrep.override { withPCRE2 = true; })
        gnutls # for TLS connectivity
        coreutils
        fd # faster projectile indexing
        imagemagick # for image-dired    sqlite
        gcc
        zstd # for undo-fu-session/undo-tree compression
        notmuch

        # shell mode
        shfmt
        shellcheck
        bash-language-server

        # web mode
        html-tidy

        # docker mode
        dockfmt

        # :tools editorconfig
        editorconfig-core-c # per-project style config

        #misc/unknown
        stylelint
        # vimPlugins.copilot-vim

        unstable.nerd-fonts.im-writing
        unstable.nerd-fonts.jetbrains-mono
        unstable.nerd-fonts.symbols-only

        # for markdown-preview-eww
        rubyPackages.redcarpet
        yaml-language-server

        # nix
        nixd

        # typescript
        eslint
        prettier
        # prettier-plugin-go-template
        typescript-language-server

        # language tools
        languagetool

        # other dependencies
        hunspell
        hunspellDicts.de_AT
        hunspellDicts.de_DE
        hunspellDicts.hu_HU
        hunspellDicts.en_US
        hunspellDicts.es_ES
        hunspellDicts.en_GB-ize

        # python
        stable.poetry
        pyright
        ruff
        python313Packages.debugpy
        python313Packages.isort
        python313Packages.pylint
        python313Packages.yapf
        python313Packages.pylama
        python313Packages.jupyter
        vscode-langservers-extracted

        xclip
        poppler-utils

        # go
        gopls
        go

        # just
        just
        # just-lsp

        #fonts
        unstable.nerd-fonts.im-writing
        unstable.nerd-fonts.jetbrains-mono
        unstable.nerd-fonts.fira-code
        unstable.nerd-fonts.caskaydia-cove
        unstable.nerd-fonts.geist-mono

        # ai
        # aider-chat

        #org mode
        mermaid-cli
      ];

      sops.secrets."emacs/authinfo" = { };

      xdg.configFile."emacs/.env".text = ''
        WORK_GITFORGE_HOST=${osConfig.work.gitForgeHost}
        EMACS_AUTHINFO_PATH=${config.sops.secrets."emacs/authinfo".path}
      '';
      programs.git.settings = {
        gitlab.${osConfig.work.gitForgeHost}.user = "${osConfig.work.gitUser}";
        gitlab."${osConfig.work.gitForgeHost}/api/v4".user = "${osConfig.work.gitUser}";
      };

      home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];
      };
}
