{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.keyd;
in {
  options.features.keyd.enable = mkEnableOption "keyd config";
  config = mkIf cfg.enable {
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
          };
        };
      };
    };
  };
}
