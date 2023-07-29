{ config, lib, pkgs, ... }:

{
  import = [
    ./picom
    ./polybar
    ./bspwm.nix
    ./sxhkd.nix
  ];

  environment.systemPackages = with pkgs; [
    st
    rofi
    polybar
    xclip
    xdotool
    xorg.xwininfo
  ];

  services.xserver = {
    layout = "us";
    xkbVariant = "altgr-intl";
    autoRepeatDelay = 250;
    autoRepeatInterval = 1000 / 60;
    enable = true;
    displayManager = {
      lightdm.enable = true;
      defaultSession = "none+bspwm";
    };
    windowManager.bspwm.enable = true;
    desktopManager.xfce.enable = true;
    resolutions = [{
      x = 1920;
      y = 1080;
    }];
  };
}
