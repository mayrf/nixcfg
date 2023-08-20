{ config, pkgs, lib, location, ... }:

{
  programs.emacs = {
    enable = true;
    package = with pkgs;
      ((emacsPackagesFor emacsNativeComp).emacsWithPackages
        (epkgs: with epkgs; [ vterm emacsql-sqlite ]));
  };
  services.emacs = {
    enable = true;
    client.enable = true;
    startWithUserSession = "graphical";
    socketActivation.enable = true;
    package = with pkgs;
      ((emacsPackagesFor emacsNativeComp).emacsWithPackages
        (epkgs: with epkgs; [ vterm emacsql-sqlite ]));
  };
  # systemd.user.services.emacs.Install.WantedBy = [ "default.target" ];

  home.activation =
    {
      doomEmacsActivationAction = lib.hm.dag.entryAfter [ "installPackages" ] ''
        EMACS="${config.home.homeDirectory}/.config/emacs"
        DOOM="${config.home.homeDirectory}/.config/doom"
        if [ ! -d "$EMACS" ]; then
          ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
          yes | $EMACS/bin/doom install
          rm -r $HOME/.config/doom
        fi
        if [ ! -d "$DOOM" ]; then
          ln -s ${config.home.homeDirectory}/.config/nixcfg/home/mayrf/features/editors/emacs/doom-emacs/doom $HOME/.config/doom
        fi
        # TODO find a way to make this work (Error: failed to run Emacs with command 'emacs'
        # Are you sure Emacs is installed and in your $PATH?
        # if [ -x "/home/mayrf/.config/emacs/bin/doom" ]; then
        #   /home/mayrf/.config/emacs/bin/doom sync
        # fi
      ''; # It will always sync when rebuild is done. So changes will always be applied.
    };

  home.packages = with pkgs; [
    # Doom emacs dependencies
    ripgrep
    coreutils
    fd
    sqlite
    gcc
    emacs-all-the-icons-fonts

    # other dependencies
    hunspell
    hunspellDicts.de_AT
    hunspellDicts.de_DE
    hunspellDicts.hu_HU
    hunspellDicts.en_US
    hunspellDicts.es_ES
    hunspellDicts.en_GB-ize
  ];
}
