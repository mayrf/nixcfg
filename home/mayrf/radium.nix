{ inputs, outputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/lf
    ./features/terminal/kitty.nix
    ./features/editors/emacs/doom-emacs
  ];

  colorscheme = inputs.nix-colors.colorschemes.atelier-sulphurpool;

  wallpaper = outputs.wallpapers.aenami-northwind;
}
