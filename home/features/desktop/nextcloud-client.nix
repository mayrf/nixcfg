{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.nextcloud-client;
in {
  options.features.desktop.nextcloud-client.enable =
    mkEnableOption "nextcloud-client config";

  config = mkIf cfg.enable {

    features.impermanence.directories =
      [ ".local/share/Nextcloud/" ".config/Nextcloud" ];
    home.packages = with pkgs; [ stable.nextcloud-client ];
    services = {
      nextcloud-client = {
        enable = true;
        package = pkgs.stable.nextcloud-client;
        startInBackground = true;
      };
    };
  };
}
