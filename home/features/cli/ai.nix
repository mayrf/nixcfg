{ config, lib, pkgs, hostSpec, ... }:
with lib;
let cfg = config.features.cli.ai;
in {
  options.features.cli.ai.enable = mkEnableOption "enable ai cli programs";

  config = mkIf cfg.enable {
    home.persistence."${hostSpec.persistDir}/system/home/${hostSpec.username}" =
      if (hostSpec.isImpermanent == true) then {
        directories = [ ".config/fabric" ];
        allowOther = false;
        # files = [ ".screenrc" ];
      } else
        { };
    home.packages = with pkgs; [ oterm fabric-ai ];
  };
}
