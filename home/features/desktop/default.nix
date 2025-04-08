{pkgs, ...}: {
  imports = [
    ./wayland.nix
    ./hyprland.nix
    ./gammastep.nix
    ./mako.nix
    ./wofi.nix
    ./waybar.nix
    ./nextcloud-client.nix
    ./virtualisation.nix
    ./email.nix
    ./postman.nix
    ./teams.nix
  ];

  home.packages = with pkgs; [
  ];
}
