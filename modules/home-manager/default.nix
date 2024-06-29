{
  # fonts = import ./fonts.nix;
  # monitors = import ./monitors.nix;
  # wallpaper = import ./wallpaper.nix;
  # lf = import ./lf;
  imports = [
   ./fonts.nix
   ./monitors.nix
   ./wallpaper.nix
   ./lf
  ];
}
