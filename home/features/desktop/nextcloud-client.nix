{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.nextcloud-client;
in {
  options.features.desktop.nextcloud-client.enable =
    mkEnableOption "nextcloud-client config";

  config = mkIf cfg.enable {
    services = {
      nextcloud-client = {
        enable = true;
        startInBackground = true;
      };
    };
  };
}
