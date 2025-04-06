{ config, pkgs, ... }: {

  services = {
    keyd = {
      enable = true;
      keyboards.${config.hostSpec.username}.settings = {
        main = { capslock = "overload(caps_layer, esc)"; };
        caps_layer = {
          j = "down";
          k = "up";
          h = "left";
          l = "right";
          "1" = "super + 1";
        };
      };
    };
  };
}
