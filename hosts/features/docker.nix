{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.docker;
in {
  options.features.docker.enable = mkEnableOption "my docker machine config";
  config = mkIf cfg.enable {

    # Enable common container config files in /etc/containers
    virtualisation = {
      containers.enable = true;

      containers.containersConf.settings = {
        engine.compose_warning_logs = false;
      };
    };
    virtualisation = {
      podman = {
        enable = true;
        dockerSocket.enable = true;

        # Create a `docker` alias for podman, to use it as a drop-in replacement
        dockerCompat = true;

        # Required for containers under podman-compose to be able to talk to each other.
        defaultNetwork.settings.dns_enabled = true;
      };
    };

    # Useful other development tools
    environment.systemPackages = with pkgs; [
      dive # look into docker image layers
      podman-tui # status of containers in the terminal
      docker-compose # start group of containers for dev
      docker-credential-helpers
      #distrobox
      #podman-compose # start group of containers for dev
    ];

    # environment.extraInit = ''
    #   if [ -z "$DOCKER_HOST" -a -n "$XDG_RUNTIME_DIR" ]; then
    #     export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
    #   fi
    # '';

      # virtualisation.containers.storage.settings.storage.driver = "btrfs";
    # virtualisation.docker.storageDriver = lib.optionals (config.features.impermanence.enable == true) "btrfs";
  };
}
