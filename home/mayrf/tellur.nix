{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./features/lf
    ./features/terminal/alacritty.nix
    ./features/desktop/hyprland
    ./features/editors/emacs/doom-emacs
    ./features/editors/vscode.nix
    ./features/postman.nix
    ./features/editors/idea.nix
  ];


  colorscheme = inputs.nix-colors.colorschemes.atelier-sulphurpool;

  wallpaper = outputs.wallpapers.aenami-cold-red-light;

  #  ------   ------
  # | DP-1 | | DP-3 |
  #  ------   ------
  #
  monitors = [
    {
      # DP-1
      name = "eDP-1";
      width = 1920;
      height = 1080;
      workspace = "1";
      primary = true;
    }
    {
      # DP-3
      name = "DP-3";
      width = 5120;
      height = 1440;
      x = 1920;
      workspace = "2";
    }
    # {
    #   # DP-3
    #   name = "HDMI-A-3";
    #   width = 1680;
    #   height = 1050;
    #   noBar = true;
    #   x = 1920;
    #   workspace = "3";
    # }
  ];
}
