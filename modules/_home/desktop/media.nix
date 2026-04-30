{ config, pkgs, ... }:
{
  features.impermanence.files = [
    ".config/kwalletrc"
  ];
  features.impermanence.directories = [
    ".local/share/kwalletd/"
    ".config/BraveSoftware"
    ".cache/BraveSoftware"
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
    sushi
  ];
}
