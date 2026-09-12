{ ... }:
{
  flake.modules.homeManager.opencloudClient =
    { config, lib, pkgs, ... }:
    {
      assertions = [
        (lib.hm.assertions.assertPlatform "services.opencloud-client" pkgs lib.platforms.linux)
      ];
      home.packages = with pkgs; [ opencloud-desktop ];

      features.impermanence.directories = [ ".config/OpenCloud" ];
      features.impermanence.directories_cache = [ ".cache/OpenCloud" ];

      systemd.user.services.opencloud-client = {
        Unit = {
          Description = "Opencloud Client";
          After = [ "graphical-session.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Environment = [ "PATH=${config.home.profileDirectory}/bin" ];
          ExecStart = "${pkgs.opencloud-desktop}/bin/opencloud";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
    };
}
