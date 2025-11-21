{ config, lib, ... }:
with lib;
let cfg = config.features.cli.syncthing;
in {
  options.features.cli.syncthing.enable = mkEnableOption "syncthing config";
  config = mkIf cfg.enable {
    features.impermanence.directories = [ ".local/state/syncthing/" ];
    services.syncthing = {
      enable = true;
      guiAddress = "127.0.0.1:8384";
      overrideDevices = false;
      overrideFolders = false;
      settings = {
        options.urAccepted = -1;
        gui = {
          enabled = true;
          tls = true; # Enable HTTPS
        };
      };
    };
    systemd.user.services.syncthing.Service.Environment = [
      "HTTP_PROXY="
      "HTTPS_PROXY="
      "http_proxy="
      "https_proxy="
      "ALL_PROXY="
      "all_proxy="
    ];
  };

}
