{ config, pkgs, ... }: {

  services = {
    keyd = {
      enable = true;
      keyboards.mayrf.settings = {
        main = { capslock = "overload(caps_layer, esc)"; };
        caps_layer = {
          j = "down";
          k = "up";
          h = "left";
          l = "right";
        };
      };
    };
  };
}
