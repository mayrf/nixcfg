{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.printing;
in {
  options.features.printing.enable = mkEnableOption "printing config";
  config = mkIf cfg.enable {
    features.impermanence.directories = [ "/var/cache/cups" "/lib/lib/cups" ];
    # Enable CUPS to print documents.
    services.printing.enable = true;
    services.printing.drivers = [
      pkgs.gutenprint
      pkgs.brlaser
      pkgs.brgenml1lpr
      pkgs.brgenml1cupswrapper
    ];

    environment.systemPackages = [
      pkgs.system-config-printer
    ];
  };

}
