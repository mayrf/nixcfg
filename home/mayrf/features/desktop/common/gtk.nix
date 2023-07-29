{ config, pkgs, inputs, ... }:

let
  gruvboxPlus = import ../../../../../pkgs/gruvbox-plus.nix { inherit pkgs; };
in
{

  xdg.configFile."gtk-4.0/gtk.css".source = ./gtk.css;
  xdg.configFile."gtk-3.0/gtk.css".source = ./gtk.css;
  gtk.enable = true;

  gtk.cursorTheme.package = pkgs.bibata-cursors;
  gtk.cursorTheme.name = "Bibata-Modern-Ice";

  gtk.theme.package = pkgs.adw-gtk3;
  gtk.theme.name = "adw-gtk3";

  gtk.iconTheme.package = gruvboxPlus;
  gtk.iconTheme.name = "GruvboxPlus";
}
