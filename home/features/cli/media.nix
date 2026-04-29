{ pkgs, ... }:
{
  home.packages = with pkgs; [
    feh # Image Viewer
    sxiv
  ];
}
