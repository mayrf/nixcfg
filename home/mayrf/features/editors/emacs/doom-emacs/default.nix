{ config, pkgs, lib, location, ... }:

{
  # Emacs
  services.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29;
  };
  #  package = with pkgs;
  #    ((emacsPackagesFor emacsNativeComp).emacsWithPackages
  #      (epkgs: with epkgs; [ vterm emacsql-sqlite ]));
  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   startWithUserSession = "graphical";
  #   socketActivation.enable = true;
  #   package = with pkgs;
  #     ((emacsPackagesFor emacsNativeComp).emacsWithPackages
  #       (epkgs: with epkgs; [ vterm emacsql-sqlite ]));
  # };
  # systemd.user.services.emacs.Install.WantedBy = [ "default.target" ];

  home.activation = {
    doomEmacsActivationAction = lib.hm.dag.entryAfter [ "writeBoundry" ] ''
      EMACS="${config.home.homeDirectory}/.config/emacs"
      DOOM="${config.home.homeDirectory}/.config/doom"
      if [ ! -d "$EMACS" ]; then
        ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
      #  yes | $EMACS/bin/doom install
        rm $HOME/.config/doom
      fi

      if [ ! -d "$DOOM" ]; then
        ln -s ${config.home.homeDirectory}/.config/nixcfg/home/mayrf/features/editors/emacs/doom-emacs/doom $HOME/.config/doom
      fi

      # $EMACS/bin/doom sync
      # TODO find a way to make this work (Error: failed to run Emacs with command 'emacs'
      # Are you sure Emacs is installed and in your $PATH?
      # if [ -x "/home/mayrf/.config/emacs/bin/doom" ]; then
      #   /home/mayrf/.config/emacs/bin/doom sync
      # fi
    ''; # It will always sync when rebuild is done. So changes will always be applied.

    #   doomEmacsSyncAction = lib.hm.dag.entryAfter [ "installPackages" ] ''
    #        EMACS="${config.home.homeDirectory}/.config/emacs"
    #        $EMACS/bin/doom sync
    #'';
  };

  home.packages = with pkgs; [
    # Doom emacs dependencies
    emacsPackages.vterm
    emacsPackages.emacsql-sqlite
    emacsPackages.pdf-tools
    git
    (ripgrep.override { withPCRE2 = true; })
    gnutls # for TLS connectivity    coreutils
    fd # faster projectile indexing
    imagemagick # for image-dired    sqlite
    gcc
    emacs-all-the-icons-fonts

    zstd # for undo-fu-session/undo-tree compression

    # :tools editorconfig
    editorconfig-core-c # per-project style config

    # other dependencies
    hunspell
    hunspellDicts.de_AT
    hunspellDicts.de_DE
    hunspellDicts.hu_HU
    hunspellDicts.en_US
    hunspellDicts.es_ES
    hunspellDicts.en_GB-ize
  ];
  home.sessionPath = [ "$XDG_CONFIG_HOME/emacs/bin" ];
}
