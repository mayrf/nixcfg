{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.docker;
in {
  options.features.docker.enable = mkEnableOption "my docker machine config";
  config = mkIf cfg.enable {
    # virtualisation.docker.enable = true;
    virtualisation.docker.rootless = {
      enable = true;
      setSocketVariable = true;
    };
    # virtualisation.containerd.enable = true;

    environment.systemPackages = with pkgs; [
      docker-compose
      distrobox
      docker-credential-helpers
    ];
    # virtualisation.podman = {
    #   enable = true;
    #   dockerCompat = true;
    # };

    # environment.systemPackages = [ pkgs.distrobox ];
  };
}
