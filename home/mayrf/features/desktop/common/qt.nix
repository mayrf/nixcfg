{ pkgs, config, stable, ... }: {

  home.packages = with pkgs; [

    stable.qt6.qtwayland
    libsForQt5.qt5.qtwayland
  ];
  # qt = {

  #   enable = true;
  #   # platform theme "gtk" or "gnome"
  #   platformTheme = "gtk";
  #   # name of the qt theme
  #   style.name = "adwaita-dark";

  #   style.package = stable.adwaita-qt;
  # };
}
