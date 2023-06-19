{ config, pkgs, location, ... }:

{
  services.emacs.enable = true;
  services.emacs.package = with pkgs;
    ((emacsPackagesFor emacs).emacsWithPackages
      (epkgs: with epkgs; [ vterm emacsql-sqlite ]));

  system.userActivationScripts =
    { # Installation script every time fixos-rebuild is run. So not during initial install.
      doomEmacs = {
        text = ''
          source ${config.system.build.setEnvironment}
          EMACS="$HOME/.config/emacs"

          if [ ! -d "$EMACS" ]; then
            ${pkgs.git}/bin/git clone https://github.com/hlissner/doom-emacs.git $EMACS
            yes | $EMACS/bin/doom install
            rm -r $HOME/.config/doom
            ln -s $HOME/nixcfg/modules/editors/emacs/doom-emacs/doom $HOME/.config/doom
            $EMACS/bin/doom sync
          else
            $EMACS/bin/doom sync
          fi
        ''; # It will always sync when rebuild is done. So changes will always be applied.
      };
    };

  environment.systemPackages = with pkgs; [
    ripgrep
    coreutils
    fd
    sqlite
    gcc
  ]; # Dependencies
}

