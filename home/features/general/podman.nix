{ pkgs, ... }:
{
  home.packages = [
    pkgs.distrobox
  ];
  services.podman.enable = true;
}
