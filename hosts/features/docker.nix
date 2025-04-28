{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.docker;
in {
  options.features.docker.enable = mkEnableOption "my docker machine config";
  config = mkIf cfg.enable {
    # virtualisation.docker.enable = true;
    virtualisation.docker = {
      enable = true;
      storageDriver = "btrfs";
      rootless = {

        # enable = false;
        enable = true;
        setSocketVariable = true;
      };
    };
    # virtualisation.docker.daemon.settings = {
    #   data-root = "/home/mayrf/.local/state/docker-images";
    # };

    # virtualisation.containerd.enable = true;

    environment.systemPackages = with pkgs; [
      docker-compose
      distrobox
      docker-credential-helpers
    ];
    # virtualisation.podman = {
    #   enable = true;
    #   dockerCompat = true;
    #   dockerSocket.enable = true;
    # };
    # Enable common container config files in /etc/containers
    # virtualisation.containers.enable = true;
    # virtualisation = {
    #   podman = {
    #     enable = true;

    #     # Create a `docker` alias for podman, to use it as a drop-in replacement
    #     dockerCompat = true;

    #     # Required for containers under podman-compose to be able to talk to each other.
    #     defaultNetwork.settings.dns_enabled = true;
    #   };
    # };

    # Useful other development tools
    # environment.systemPackages = with pkgs; [
    #   dive # look into docker image layers
    #   podman-tui # status of containers in the terminal
    #   docker-compose # start group of containers for dev
    #   #podman-compose # start group of containers for dev
    # ];

    # environment.systemPackages = [ pkgs.distrobox ];
  };
}
