{ config, pkgs, inputs, ... }:

let
  gruvboxPlus = import ../../../../../pkgs/gruvbox-plus.nix { inherit pkgs; };
  inherit (inputs.nix-colors.lib-contrib { inherit pkgs; }) gtkThemeFromScheme;
in {

  # xdg.configFile."gtk-4.0/gtk.css".source = ./gtk.css;
  xdg.configFile."gtk-3.0/gtk.css".source = ./gtk.css;
  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     package = pkgs.rose-pine-icon-theme;
  #     name = "rose-pine-icon-theme";
  #   };
  #   theme = {
  #     name = "Catppuccin-Macchiato-Compact-Pink-Dark";
  #     package = pkgs.catppuccin-gtk.override {
  #       accents = [ "pink" ];
  #       size = "compact";
  #       # tweaks = [ "rimless" "black" ];
  #       variant = "frappe";
  #     };
  #   };
  # };
  gtk = {
    enable = true;
    theme = {
      name = "${config.colorScheme.slug}";
      package = gtkThemeFromScheme { scheme = config.colorScheme; };
    };
    iconTheme = {
      package = pkgs.zafiro-icons;
      name = "Zafiro-icons-Dark";
    };
    cursorTheme = {
      package = pkgs.graphite-cursors;
      name = "graphite-dark";
      size = 17;
    };
  };
  # Now symlink the `~/.config/gtk-4.0/` folder declaratively:
  # xdg.configFile = {
  #   "gtk-4.0/assets".source =
  #     "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/assets";
  #   "gtk-4.0/gtk.css".source =
  #     "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk.css";
  #   "gtk-4.0/gtk-dark.css".source =
  #     "${config.gtk.theme.package}/share/themes/${config.gtk.theme.name}/gtk-4.0/gtk-dark.css";
  # };
}
