{ ... }:
{
  flake.modules.homeManager.podman =
    { pkgs, ... }:
    {
      home.packages = with pkgs; [
        distrobox
        dive
        podman-tui
        docker-compose
      ];
      services.podman.enable = true;
    };
}
