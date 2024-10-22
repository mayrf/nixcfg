{ config, pkgs, lib, ... }:
with lib;
let cfg = config.mymodules.docker;
in {
  options.mymodules.docker = {
    enable = mkEnableOption "my docker machine config";
  };
  config = mkIf cfg.enable {
    virtualisation.docker.enable = true;
    virtualisation.docker.rootless = {
      enable = true;
      setSocketVariable = true;
    };
    # virtualisation.containerd.enable = true;

    environment.systemPackages = with pkgs; [
      docker-compose
      docker-credential-helpers
    ];
  };
}
