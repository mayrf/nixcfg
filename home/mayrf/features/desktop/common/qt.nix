{ pkgs, config, pkgs-stable, ... }: {

  home.packages = with pkgs; [

    pkgs-stable.qt6.qtwayland
    libsForQt5.qt5.qtwayland
  ];
  # qt = {

  #   enable = true;
  #   # platform theme "gtk" or "gnome"
  #   platformTheme = "gtk";
  #   # name of the qt theme
  #   style.name = "adwaita-dark";

  #   style.package = pkgs-stable.adwaita-qt;
  # };
}
