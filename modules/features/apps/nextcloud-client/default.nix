{ ... }:
{
  flake.modules.homeManager.nextcloudClient =
    { config, pkgs, ... }:
    {
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
