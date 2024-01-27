{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/lf
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
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
  monitors = [{
    # DP-1
    name = "HDMI-A-1";
    width = 1920;
    height = 1080;
    workspace = "1";
    primary = true;
  }];
}
