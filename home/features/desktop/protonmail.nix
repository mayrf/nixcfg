{ config, pkgs, lib, configVars, ... }:
with lib;
let cfg = config.features.desktop.protonmail;
in {
  options.features.desktop.protonmail.enable = mkEnableOption "my protonmail user config"; 
  config = mkIf cfg.enable {

    home.packages = [
      pkgs.stable.protonmail-bridge
      pkgs.stable.protonmail-bridge-gui
      pkgs.protonmail-desktop
    ];
    home.persistence."${configVars.persistDir}/home/${configVars.username}" = {
      directories = [ ".local/share/protonmail" ".config/Proton Mail" ];
      allowOther = false;
      # files = [ ".screenrc" ];
    };
  };
}
