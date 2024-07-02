{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/lf
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
    ./features/desktop/hyprland
    ./features/editors/emacs
    ./features/editors/vscode.nix
    ./features/postman.nix
    ./features/editors/idea.nix
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  #  ------   ------
  # | DP-1 | | DP-3 |
  #  ------   ------
  #
  monitors = [{
    # DP-1
    name = "HDMI-A-1";
    width = 2560;
    height = 1440;
    workspace = "1";
    primary = true;
  }];
}
