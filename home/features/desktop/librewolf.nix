{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.librewolf;
in {
  options.features.desktop.librewolf.enable = mkEnableOption "librewolf config";

  config = mkIf cfg.enable {
    programs.librewolf.enable = true;
  };
}
