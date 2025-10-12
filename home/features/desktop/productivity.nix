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
      ".wine"
      ".config/immich"
    ];

    home.packages = with pkgs; [
      stable.calibre
      drawio
      thunderbird
      obsidian
      krita
      inkscape
      reaper
      scribus
      gimp
      stable.gnucash
      mmex
      stable.ardour
      onlyoffice-bin
      temurin-jre-bin-21
      libreoffice-qt6-fresh
      keepassxc
      # pkgs-stable.calibre
      # gramps
      legcord
      ipscan
      dbgate
      wine
      immich-cli
    ];
  };
}
