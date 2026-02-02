{ config, pkgs, lib, private, hostSpec, ... }:
with lib;
let
  cfg = config.features.editor.emacs;

  emacs = ((pkgs.emacsPackagesFor pkgs.emacs-git).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.emacsql epkgs.pdf-tools epkgs.org epkgs.treesit-grammars.with-all-grammars ]));
  repoUrl = "https://github.com/doomemacs/doomemacs";
  emacsBinPath = "${emacs}/bin";
  # Match the default socket path for the Emacs version so emacsclient continues
  # to work without wrapping it.
  socketDir = "%t/emacs";
in {
  options.features.editor.emacs.enable = mkEnableOption "my emacs user config";
  config = mkIf cfg.enable {
    features.impermanence.directories = [
      # ".config/emacs"
      ".config/emacs-doom"
      ".config/dotemacs"
    ];

    # home.file."Documents/org/shared/.config/enchant".source = config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/enchant";
    home.file."${config.xdg.configHome}/enchant".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/Documents/org/shared/.config/enchant";

    # Emacs
    services.emacs = {
      enable = true;
      package = emacs;
    };

    programs.emacs = {
      enable = true;
      # package = emacs;
    };

    home.activation = let
      source = "${hostSpec.flakeDir}/home/features/editor/emacs/vanilla";
      target = "${config.xdg.configHome}/emacs";
    in {
      emacsActivationAction = ''
                link_repo() {
        	  rm -r ${target}
                  ln -sf ${source} ${target}
                }
                if [ ! -d "${target}" ] || [ -z "$(ls "${target}")" ]; then 
                    link_repo
                elif [ ! -L "${target}" ]; then
                    TEMP_DIR=$(mktemp -d)
                    mv ${target}/{.,}* $TEMP_DIR
                    link_repo
                    mv $TEMP_DIR/{.,}* ${target}
                    rm -r $TEMP_DIR
                fi
      '';
      doomEmacsActivationAction = ''
        check_dir() {
            local dir="$1"
            [ ! -d "$dir" ] || [ -z "$(ls "$dir")" ]
        }

        # EMACS_DIR="${config.xdg.configHome}/emacs"
        DOOM_EMACS_DIR="${config.xdg.configHome}/emacs-doom"
        DOOM="${config.xdg.configHome}/doom"

        if check_dir "$DOOM_EMACS_DIR"; then
            rm -rf $DOOM_EMACS_DIR/*
            ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" $DOOM_EMACS_DIR
        fi


        # if check_dir "$EMACS_DIR"; then
        #   ln -s ${hostSpec.flakeDir}/home/features/editor/emacs/vanilla $EMACS_DIR
        # fi

        if [ ! -e "$DOOM" ]; then
          ln -s ${hostSpec.flakeDir}/home/features/editor/emacs/doom $DOOM
          # yes | $EMACS_DIR/bin/doom install
        fi
      '';
    };
    home.sessionVariables = {
      EMACS_DIR = "${config.xdg.configHome}/emacs";
      DOOM = "${config.xdg.configHome}/doom";
      DOOMDIR = "${config.xdg.configHome}/doom";
    };
    # fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

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
      nodePackages_latest.eslint
      nodePackages_latest.prettier
      # prettier-plugin-go-template
      nodePackages_latest.typescript-language-server

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

    # if check_dir "$EMACS_DIR"; then
    #   ln -s ${hostSpec.flakeDir}/modules/home-manager/emacs/vanilla $EMACS_DIR
    # fi

    xdg.configFile."emacs/.env".text = ''
      WORK_GITFORGE_HOST=${private.work.gitForgeHost}
      EMACS_AUTHINFO_PATH=${config.sops.secrets."emacs/authinfo".path}
    '';
    programs.git.settings = {
      gitlab.${private.work.gitForgeHost}.user = "${private.work.gitUser}";
    };

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    home.shellAliases = {
      "emacs" = "${emacs}/bin/emacs";
      "doom-emacs" =
        "${emacs}/bin/emacs --init-directory ${config.xdg.configHome}/emacs-doom &";
    };
    # e()     { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
    # ediff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
    # eman()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
    # ekill() { emacsclient --eval '(kill-emacs)'; }
  };
}
