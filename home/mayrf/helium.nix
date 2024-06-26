{ inputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/lf
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
    ./features/desktop/hyprland
    ./features/terminal/kitty.nix
    ./features/editors/emacs
    ./features/editors/vscode.nix
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  #  ------   ------
  # | DP-2 | | DP-3 |
  #  ------   ------
  #  ------
  # | DP-1 |
  #  ------
  monitors = [{
    # DP-1
    name = "LVDS-1";
    width = 1366;
    height = 768;
    # y = 1080;
    workspace = "1";
    primary = true;
  }
  # {
  #   # DP-2
  #   name = "VGA-1";
  #   width = 1920;
  #   height = 1080;
  #   workspace = "2";
  # }
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
