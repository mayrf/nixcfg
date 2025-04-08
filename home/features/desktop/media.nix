{ config, lib, ... }:
with lib;
let cfg = config.features.desktop.media;
in {
  options.features.desktop.media.enable =
    mkEnableOption "enable media gui programs";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      minitube
      mpv
      deluge
      freetube
      tor-browser
      vlc
      brave
      firefox
      nautilus
      sushi # A quick previewer for Nautilus
    ];
  };
}
