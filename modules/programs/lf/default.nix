{ config, pkgs, ... }:

{

  programs.lf = {
    enable = true;
    settings = {
      color256 = true;
      icons = true;
      drawbox = true;
      hidden = true;
      ignorecase = true;
      number = true;
      preview = true;
      ratios = "2:4:3";
      relativenumber = true;
      scrolloff = 10;
      dirfirst = true;
      # sortby = "";
    };
  };
  home.file = {
    ".config/lf/icons".source = ./icons;
    ".config/lf/colors".source = ./colors;
  };
}
