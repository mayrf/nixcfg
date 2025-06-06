{ inputs, ... }:

{
  imports = [
    ../common
    ../features
    ../features/cli
    ../features/desktop
    ../features/terminal
  ];

  features = {
    cli = {
      zsh.enable = true;
      fzf.enable = true;
      yazi.enable = true;
      git.enable = true;
    };
    desktop = {
      wayland.enable = true;
      waybar.enable = true;
      hyprland.enable = true;
      gammastep.enable = true;
      mako.enable = true;
      wofi.enable = true;
      librewolf.enable = true;
      gpg.enable = true;
    };
    terminal = {
      ghostty.enable = true;
    };
  };



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
