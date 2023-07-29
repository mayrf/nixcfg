{ pkgs, config, ... }:
{
  qt = {

    enable = true;
    # platform theme "gtk" or "gnome"
    platformTheme = "gtk";
    # name of the qt theme
    style.name = "adwaita-dark";

    style.package = pkgs.adwaita-qt;
  };
}
