{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./features/linux
    ./features/lf
    ./features/teams.nix
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
    ./features/terminal/kitty.nix
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
      x = 1920;
      primary = true;
    }
    {
      # DP-3
      name = "DP-3";
      width = 5120;
      height = 1440;
      x = 1920 + 1920;
      workspace = "2";
    }
    {
      # DP-3
      name = "HDMI-A-1";
      width = 1920;
      height = 1080;
      workspace = "3";
    }
  ];
}
