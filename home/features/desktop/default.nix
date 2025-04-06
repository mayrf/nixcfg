{pkgs, ...}: {
  imports = [
    ./wayland.nix
    ./hyprland.nix
    ./gammastep.nix
    ./mako.nix
    ./wofi.nix
    ./waybar.nix
  ];

  home.packages = with pkgs; [
  ];
}
