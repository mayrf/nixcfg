{ config, lib, pkgs, inputs, outputs, ... }:

{
  imports = [
    ./global
    ./linux
    ../features/cli
    ./features/terminal/alacritty.nix
    ./features/terminal/foot.nix
    ./features/desktop/hyprland
    ./features/postman.nix
    "${inputs.dotfiles-private}/home/desktop-apps.nix"
  ];

  colorscheme = inputs.nix-colors.colorschemes.woodland;

  lf.enable = true;
  vscode.enable = true;
  emacs.enable = true;
  git.enable = true;
  email.enable = true;

  #  ------   ------
  # | DP-1 | | DP-3 |
  #  ------   ------
  #
  monitors = [
    {
      # DP-1
      name = "HDMI-A-1";
      width = 2560;
      height = 1440;
      x = 0;
      y = 0;
      workspace = "1";
      primary = true;
    }
    {
      # DP-1
      name = "DP-2";
      width = 2560;
      height = 1440;
      workspace = "1";
      x = 2560;
      y = -560;
      transform = "3";
      # primary = true;
    }
  ];
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
