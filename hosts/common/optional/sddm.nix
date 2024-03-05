{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    libsForQt5.qt5.qtquickcontrols2
    libsForQt5.qt5.qtgraphicaleffects
  ];

  programs.hyprland.enable = true; # only here to be usable as default session. Configuration in Home Manager
  services.xserver = {
    enable = true;
    displayManager = {
      defaultSession = "hyprland";
      sddm.enable = true;
      sddm.theme = "${ import ./../../../pkgs/sddm-theme.nix { inherit pkgs; }}";
    };
  };
}
