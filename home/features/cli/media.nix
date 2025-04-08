{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.media;
in {
  options.features.cli.media.enable =
    mkEnableOption "enable media cli programs";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      feh # Image Viewer
      sxiv
    ];
  };
}
