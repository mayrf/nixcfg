{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.learning;
in {
  options.features.desktop.learning.enable =
    mkEnableOption "enable learning gui programs";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      anki-bin
      tipp10
    ];
  };
}
