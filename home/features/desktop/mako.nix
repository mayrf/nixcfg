{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.mako;
in {
  options.features.desktop.mako.enable = mkEnableOption "mako config";

  config = mkIf cfg.enable {
    services.mako = {
      enable = true;
      padding = "10,20";
      anchor = "top-left";
      width = 400;
      height = 150;
      borderSize = 2;
      defaultTimeout = 12000;
    };
  };
}
