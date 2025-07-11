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
      docker.enable = false; # Disable Docker
      podman = {
        enable = true;
        dockerCompat = true; # Creates 'docker' command alias
        defaultNetwork.settings.dns_enabled = true; # Fixes DNS
      };
    };
    security.pki.certificates = [ ];
    environment.extraInit = ''
      if [ -z "$DOCKER_HOST" -a -n "$XDG_RUNTIME_DIR" ]; then
        export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
      fi
    '';
    # Useful other development tools
    environment.systemPackages = with pkgs; [
      dive # look into docker image layers
      podman-tui # status of containers in the terminal
      docker-compose # start group of containers for dev
      docker-credential-helpers
      #distrobox
    ];

    # virtualisation.docker = {
    #   enable = true;
    #   rootless = {
    #     enable = true;
    #     setSocketVariable = true;
    #     daemon.settings = { dns = [ "8.8.8.8" "1.1.1.1" ]; };
    #   };
    # };

    # virtualisation.containers.storage.settings.storage.driver = "btrfs";
    # virtualisation.docker.storageDriver = lib.optionals (config.features.impermanence.enable == true) "btrfs";

  };
}
