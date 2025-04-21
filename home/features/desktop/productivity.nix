{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.productivity;
in {
  options.features.desktop.productivity.enable =
    mkEnableOption "enable productivity gui programs";

  config = mkIf cfg.enable {

    features.impermanence.directories = [
          ".config/calibre"
          ".thunderbird"
          ".local/share/gnucash"
          ".config/keepassxc"
          ".cache/keepassxc"
          ".config/libreoffice"
    ];

    home.packages = with pkgs; [
      calibre
      drawio
      thunderbird
      obsidian
      krita
      inkscape
      reaper
      scribus
      gimp
      stable.gnucash
      stable.ardour
      onlyoffice-bin
      temurin-jre-bin-21
      libreoffice-qt6-fresh
      keepassxc
      # pkgs-stable.calibre
      legcord
      ipscan
      dbgate
    ];
  };
}
