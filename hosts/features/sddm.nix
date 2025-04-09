{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.sddm;
in {
  options.features.sddm.enable = mkEnableOption "sddm config";
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libsForQt5.qt5.qtquickcontrols2
      libsForQt5.qt5.qtgraphicaleffects
    ];

    programs.hyprland.enable =
      true; # only here to be usable as default session. Configuration in Home Manager
    services = {
      displayManager = {
        enable = true;
        defaultSession = "hyprland";
        sddm.wayland.enable = true;
        sddm.theme =
          "${import ./../../../pkgs/sddm-theme.nix { inherit pkgs; }}";
      };
    };
  };

}
