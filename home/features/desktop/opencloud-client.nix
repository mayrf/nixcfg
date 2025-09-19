{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.opencloud-client;
in {
  options.features.desktop.opencloud-client = {
    
      enable = lib.mkEnableOption "Opencloud Client";

      package = lib.mkPackageOption pkgs "opencloud-desktop" { };

      # startInBackground = lib.mkOption {
      #   type = lib.types.bool;
      #   default = false;
      #   description = "Whether to start the Opencloud client in the background.";
      # };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "services.opencloud-client" pkgs lib.platforms.linux)
    ];
    home.packages = with pkgs; [ opencloud-desktop ];

    features.impermanence.directories =
      [ ".config/OpenCloud" ];


    features.impermanence.directories_cache =
      [ ".cache/OpenCloud" ];

    systemd.user.services.opencloud-client = {
      Unit = {
        Description = "Opencloud Client";
        After = [ "graphical-session.target" ];
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Environment = [ "PATH=${config.home.profileDirectory}/bin" ];
        ExecStart =
          "${cfg.package}/bin/opencloud";
          # + (lib.optionalString cfg.startInBackground " --background");
      };

      Install = {
        WantedBy = [ "graphical-session.target" ];
      };
    };
  };
}
