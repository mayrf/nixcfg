{ inputs, ... }:

{
  imports = [
    ./global
    ./linux
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
    ./features/terminal/kitty.nix
  ];

  lf.enable = true;
  vscode.enable = true;
  emacs.enable = true;
  git.enable = true;

  colorscheme = inputs.nix-colors.colorschemes.woodland;
  wayland.windowManager.hyprland = {
    settings = {
      monitor = [ "LVDS-1,1366x768@60,0x0,1" ];
      workspace = [
        "1, monitor:LVDS-1, default:true"
        "2, monitor:LVDS-1"
        "3, monitor:LVDS-1"
        "4, monitor:LVDS-1"
        "5, monitor:LVDS-1"
        "6, monitor:LVDS-1"
        "7, monitor:LVDS-1"
      ];
    };
  };

}
