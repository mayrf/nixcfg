{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./linux
    ../features/cli
    ../features/desktop
    ../features/terminal
    ./features/postman.nix
    "${inputs.dotfiles-private}/home/desktop-apps.nix"
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;
  features = {
    cli = {
      zsh.enable = true;
      fzf.enable = true;
    };
    desktop = {
      wayland.enable = true;
      waybar.enable = true;
      hyprland.enable = true;
      gammastep.enable = true;
      mako.enable = true;
      wofi.enable = true;
    };
    terminal = {
      alacritty.enable = true;
      foot.enable = true;
    };
  };

  lf.enable = true;
  vscode.enable = true;
  emacs.enable = true;
  git.enable = true;
  email.enable = true;
  wayland.windowManager.hyprland = {
    settings = {
      monitor = [
        "HDMI-A-1,2560x1440@60,0x0,1"
        "DP-2,2560x1440@60,2560x-560,1,transform,3"
      ];
      workspace = [
        "1, monitor:HDMI-A-1, default:true"
        "2, monitor:HDMI-A-1"
        "3, monitor:HDMI-A-1"
        "4, monitor:DP-2"
        "5, monitor:HDMI-A-1"
        "6, monitor:DP-2"
        "7, monitor:DP-2"
      ];
    };
  };
  #  home.persistence."/persist/home" = {
  #    directories = [
  #      "Downloads"
  #      "Music"
  #      "Pictures"
  #      "Documents"
  #      "Videos"
  #      "VirtualBox VMs"
  #      ".gnupg"
  #      ".ssh"
  #      ".nixops"
  #      ".local/share/keyrings"
  #      ".local/share/direnv"
  #      {
  #        directory = ".local/share/Steam";
  #        method = "symlink";
  #      }
  #    ];
  #    files = [ ".screenrc" ];
  #    allowOther = true;
  #  };
}
