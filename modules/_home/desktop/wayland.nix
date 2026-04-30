{ config, pkgs, ... }:
{
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
  ];
  xdg.mimeApps.enable = true;
  home.sessionVariables = {
    MOZ_ENABLE_WAYLAND = 1;
    QT_QPA_PLATFORM = "wayland";
    LIBSEAT_BACKEND = "logind";
  };
}
