{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.wayland;
in {
  options.features.desktop.wayland.enable =
    mkEnableOption "wayland extra tools and config";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      grim
      hyprlock
      qt6.qtwayland
      slurp
      waypipe
      wf-recorder
      wl-mirror
      wl-clipboard
      wlogout
      wtype
      ydotool

      # gtk3 # For gtk-launch
      # imv
      # mimeo
      # pulseaudio
    ];
    xdg.mimeApps.enable = true;
    home.sessionVariables = {
      MOZ_ENABLE_WAYLAND = 1;
      QT_QPA_PLATFORM = "wayland";
      LIBSEAT_BACKEND = "logind";
    };
  };
}
