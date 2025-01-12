{ config, pkgs, lib, stable, configVars, ... }:
with lib;
let cfg = config.myProton;
in {
  options.myProton = { enable = mkEnableOption "my protonmail user config"; };
  config = mkIf cfg.enable {

    home.packages = [
      stable.protonmail-bridge
      stable.protonmail-bridge-gui
      pkgs.protonmail-desktop
    ];
    home.persistence."${configVars.persistDir}/home/${configVars.username}" = {
      directories = [ ".local/share/protonmail" ".config/Proton Mail" ];
      # files = [ ".screenrc" ];
    };
  };
}
