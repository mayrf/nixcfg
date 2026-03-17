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

    features.impermanence.directories =
      [ "var/lib/containers/" ];
    virtualisation = {
      docker.enable = true;
    };


    security.pki.certificates = [ ];
    # Useful other development tools
    environment.systemPackages = with pkgs; [
      dive # look into docker image layers
      docker-compose # start group of containers for dev
      docker-credential-helpers
      #distrobox
    ];

    # virtualisation.docker = {
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
