{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.librewolf;
in {
  options.features.desktop.librewolf.enable = mkEnableOption "librewolf config";

  config = mkIf cfg.enable {
    features.impermanence.directories = [ ".librewolf" ".mozilla" ];
    programs.librewolf.enable = true;
  };
}
