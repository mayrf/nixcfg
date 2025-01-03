{ config, pkgs, unstable, lib, host, configVars, ... }:
with lib;
let
  cfg = config.emacs;

  # package = pkgs.emacs-unstable;
  # emacs = pkgs.emacs-unstable.override {
  # withXwidgets = true;
  # withGTK3 = true;
  # };
  # emacs = ((pkgs.emacsPackagesFor pkgs.emacs29).emacsWithPackages
  emacs = ((pkgs.emacsPackagesFor pkgs.emacs30).emacsWithPackages
    # (epkgs: [ epkgs.vterm epkgs.emacsql-sqlite epkgs.pdf-tools ]));
    (epkgs: [ epkgs.vterm epkgs.emacsql epkgs.pdf-tools ]));
  repoUrl = "https://github.com/doomemacs/doomemacs";
  flakeDir =
    if host != "yttrium" then "~/.config/nixcfg" else "${configVars.flakeDir}";
in {
  options.emacs = { enable = mkEnableOption "my emacs user config"; };
  config = mkIf cfg.enable {

    # Emacs
    services.emacs = {
      enable = true;
      package = emacs;
    };

    programs.emacs = {
      enable = true;
      package = emacs;
    };

    home.activation = {
      doomEmacsActivationAction = ''
        check_dir() {
            local dir="$1"
            [ ! -d "$dir" ] || [ -z "$(ls "$dir")" ]
        }

        VANILLA_EMACS_DIR="${config.xdg.configHome}/emacs-vanilla"
        EMACS_DIR="${config.xdg.configHome}/emacs"
        DOOM="${config.xdg.configHome}/doom"

        if check_dir "$EMACS_DIR"; then
            rm -rf $EMACS_DIR/*
            ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" $EMACS_DIR
        fi


        if check_dir "$VANILLA_EMACS_DIR"; then
          ln -s ${flakeDir}/modules/home-manager/emacs/vanilla-emacs $VANILLA_EMACS_DIR
        fi

        if [ ! -e "$DOOM" ]; then
          ln -s ${flakeDir}/modules/home-manager/emacs/doom $DOOM
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
      vimPlugins.copilot-vim

      unstable.nerd-fonts.im-writing
      unstable.nerd-fonts.jetbrains-mono
      unstable.nerd-fonts.symbols-only

      # :markdown preview
      python311Packages.grip

      # for markdown-preview-eww
      rubyPackages.redcarpet
      yaml-language-server

      # nix
      nil

      # typescript
      nodePackages_latest.eslint
      nodePackages_latest.prettier
      prettier-plugin-go-template
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
      poetry
      ruff
      python311Packages.isort
      python311Packages.pylint
      python311Packages.yapf
      python311Packages.pylama
      vscode-langservers-extracted

      xclip
    ];

    home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    home.shellAliases = {
      "emacs" = "${emacs}/bin/emacs";
      "vanilla-emacs" =
        "${emacs}/bin/emacs --init-directory ${config.xdg.configHome}/emacs-vanilla &";
    };
    # e()     { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
    # ediff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
    # eman()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
    # ekill() { emacsclient --eval '(kill-emacs)'; }
  };
}

