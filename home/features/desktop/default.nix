{ pkgs, ... }: {
  imports = [
    ./wayland.nix
    ./hyprland.nix
    ./gammastep.nix
    ./mako.nix
    ./wofi.nix
    ./waybar.nix
    ./nextcloud-client.nix
    ./opencloud-client.nix
    ./virtualisation.nix
    ./email.nix
    ./postman.nix
    ./teams.nix
    ./zathura.nix
    ./librewolf.nix
    ./productivity.nix
    ./gpg.nix
    ./learning.nix
    ./media.nix
    ./social.nix
    ./protonmail.nix
    ./fonts.nix
    ./zen-browser.nix
  ];

  stylix.targets.emacs.enable = false;
  stylix.targets.qt.enable = false;
  home.packages = with pkgs; [
    pinentry-qt # GnuPG s interface to passphrase input
  ];
}
