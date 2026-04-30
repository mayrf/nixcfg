{ config, pkgs, host, ... }:
{
  home.packages = [
    pkgs.stable.protonmail-bridge
    pkgs.stable.protonmail-bridge-gui
    pkgs.protonmail-desktop
  ];
  features.impermanence.directories = [
    ".local/share/protonmail"
    ".config/Proton Mail"
  ];
}
