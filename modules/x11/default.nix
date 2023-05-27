{ config, lib, pkgs, ... }:

{
  services.xserver = {
    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "compose:menu";
    autoRepeatDelay = 250;
    autoRepeatInterval = 1000 / 60;
    enable = true;
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+bspwm";
    };
    windowManager.bspwm.enable = true;
    desktopManager.xfce.enable = true;
  };
}
