{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.social;
in {
  options.features.desktop.social.enable =
    mkEnableOption "enable social gui programs";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
        signal-desktop-source
        hexchat
    ];
  };
}
