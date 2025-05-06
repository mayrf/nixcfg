{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.podman;
in {
  options.features.podman.enable = mkEnableOption "podman config";

  config = mkIf cfg.enable {
    services.podman.enable = true;

    home.packages = with pkgs; [
      dive # look into docker image layers
      podman-tui # status of containers in the terminal
      docker-compose # start group of containers for dev
      #podman-compose # start group of containers for dev
    ];
  };
}
