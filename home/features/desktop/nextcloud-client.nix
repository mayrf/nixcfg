{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.features.desktop.nextcloud-client;
in
{
  options.features.desktop.nextcloud-client.enable = mkEnableOption "nextcloud-client config";

  config = mkIf cfg.enable {
    features.impermanence.directories = [ ".config/Nextcloud" ];
    features.impermanence.directories_cache = [ ".cache/Nextcloud" ];
    home.packages = with pkgs; [ nextcloud-client ];
    services = {
      nextcloud-client = {
        enable = true;
        package = pkgs.nextcloud-client;
        startInBackground = true;
      };
    };
  };
}
