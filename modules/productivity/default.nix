{ ... }:
{
  flake.modules.homeManager.productivity =
    { config, pkgs, ... }:
    {
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
        inkscape
        reaper
        stable.scribus
        gimp
        stable.gnucash
        mmex
        stable.ardour
        onlyoffice-desktopeditors
        temurin-jre-bin-21
        libreoffice-qt6-fresh
        keepassxc
        legcord
        stable.ipscan
        dbgate
        wine
        immich-cli
      ];
    };
}
