{ config, pkgs, lib, location, ... }:
let
  emacs = pkgs.emacs29; # pkgs.emacs-macport
  # package = pkgs.emacs-unstable;
  # emacs = pkgs.emacs-unstable.override {
  # withXwidgets = true;
  # withGTK3 = true;
  # };
  repoUrl = "https://github.com/doomemacs/doomemacs";
in {
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
          rm -rf $EMACS_DIR
          ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" $EMACS_DIR
      fi


      if check_dir "$VANILLA_EMACS_DIR"; then
        ln -s ${config.xdg.configHome}/nixcfg/home/mayrf/features/editors/emacs/vanilla-emacs $VANILLA_EMACS_DIR
      fi

      if [ ! -e "$DOOM" ]; then
        ln -s ${config.xdg.configHome}/nixcfg/home/mayrf/features/editors/emacs/doom $DOOM
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
    emacsPackages.vterm
    emacsPackages.emacsql-sqlite
    emacsPackages.pdf-tools
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
    vscode-langservers-extracted

    # docker mode
    dockfmt

    # :tools editorconfig
    editorconfig-core-c # per-project style config

    #misc/unknown
    stylelint
    vimPlugins.copilot-vim

    (pkgs.nerdfonts.override {
      fonts = [ "NerdFontsSymbolsOnly" "JetBrainsMono" "iA-Writer" ];
    })

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
    nodePackages_latest.prettier
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
}
