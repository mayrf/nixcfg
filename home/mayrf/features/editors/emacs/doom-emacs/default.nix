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
      EMACS_DIR="${config.xdg.configHome}/emacs"
      DOOM="${config.xdg.configHome}/doom"
      if [ ! -d "$EMACS_DIR" ] || [ -z "$(ls  "$EMACS_DIR")" ]; then
          rm -rf $EMACS_DIR
          ${pkgs.git}/bin/git clone --depth=1 --single-branch "${repoUrl}" $EMACS_DIR
      fi

      if [ ! -e "$DOOM" ]; then
        ln -s ${config.xdg.configHome}/nixcfg/home/mayrf/features/editors/emacs/doom-emacs/doom $DOOM
        # yes | $EMACS_DIR/bin/doom install
      fi
    '';
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

    shfmt
    shellcheck

    html-tidy
    vscode-langservers-extracted
    nodePackages_latest.bash-language-server

    stylelint

    dockfmt

    (pkgs.nerdfonts.override {
      fonts = [ "NerdFontsSymbolsOnly" "JetBrainsMono" "iA-Writer" ];
    })

    zstd # for undo-fu-session/undo-tree compression

    # :tools editorconfig
    editorconfig-core-c # per-project style config

    # :markdown preview
    python311Packages.grip

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

    #python
    poetry
    nodePackages_latest.pyright
    python311Packages.isort
    python311Packages.pylint
    python311Packages.yapf
    python311Packages.pylama
    nodePackages_latest.vscode-css-languageserver-bin

    # for markdown-preview-eww
    rubyPackages.redcarpet

  ];

  home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];

  home.shellAliases = { "emacs" = "${emacs}/bin/emacs"; };
  # e()     { pgrep emacs && emacsclient -n "$@" || emacs -nw "$@" }
  # ediff() { emacs -nw --eval "(ediff-files \"$1\" \"$2\")"; }
  # eman()  { emacs -nw --eval "(switch-to-buffer (man \"$1\"))"; }
  # ekill() { emacsclient --eval '(kill-emacs)'; }
}
