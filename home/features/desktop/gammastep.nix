{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.gammastep;
in {
  options.features.desktop.gammastep.enable = mkEnableOption "gammastep config";

  config = mkIf cfg.enable {
    services.gammastep = {
      enable = true;
      provider = "geoclue2";
      temperature = {
        day = 6000;
        night = 4600;
      };
      settings = { general.adjustment-method = "wayland"; };
    };
  };
}
