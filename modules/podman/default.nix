{ ... }:
{
  flake.modules.homeManager.podman =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.distrobox
      ];
      services.podman.enable = true;
    };
}
