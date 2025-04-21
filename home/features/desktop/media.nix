{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.media;
in {
  options.features.desktop.media.enable =
    mkEnableOption "enable media gui programs";

  config = mkIf cfg.enable {

    features.impermanence.files = [
      # Brave
      ".config/kwalletrc"
    ];
    features.impermanence.directories = [
      # Brave
      ".local/share/kwalletd/"
      ".config/BraveSoftware"
      ".cache/BraveSoftware"

      # For nautilus bookmarks
      # ".config/gtk-3.0/bookmarks"
      ".config/gtk-3.0"
      ".local/share/nautilus"

      ".config/FreeTube"
    ];
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
