{ config, pkgs, ... }:

{
  xdg.configFile = {
    "lf/lfrc".source = ./lfrc;
    "lf/icons".source = ./icons;
    "lf/colors".source = ./colors;
  };

  home.packages = with pkgs; [
    # lf dependencies
    ctpv
    ueberzugpp
    ffmpeg_6-full
    jq
    poppler_utils
    bat
  ];

  programs.lf = {
    enable = true;
  };
}
