{ config, pkgs, ... }: {
  system.userActivationScripts =
    { # Installation script every time nixos-rebuild is run. So not during initial install.
      xcompose = {
        text = ''
          XCOMPOSE="$HOME/.XCompose"
          if [ ! -h "$XCOMPOSE" ]; then
            ln -s $HOME/nixcfg/config/compose/XCompose $XCOMPOSE
          fi
        ''; # It will always sync when rebuild is done. So changes will always be applied.
      };
    };
  services = {
    keyd = {
      enable = true;
      settings = {
        main = { capslock = "overload(caps_layer, esc)"; };
        caps_layer = {
          j = "down";
          k = "up";
          h = "left";
          l = "right";
        };
        altgr = {
          a = ''macro(compose a ")'';
          q = "macro(compose a ')";

          u = ''macro(compose u ")'';
          y = "macro(compose u ')";
          j = "macro(compose u u)";

          o = ''macro(compose o ")'';
          p = "macro(compose o ')";
          l = "macro(compose o o)";
        };
        "altgr+shift" = {

          a = ''macro(compose A ")'';
          q = "macro(compose A ')";

          u = ''macro(compose U ")'';
          y = "macro(compose U ')";
          j = "macro(compose U U)";

          o = ''macro(compose O ")'';
          p = "macro(compose O ')";
          l = "macro(compose O O)";
        };
      };
    };
  };
}
