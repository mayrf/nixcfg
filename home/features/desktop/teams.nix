{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.teams;
in {
  options.features.desktop.teams.enable = mkEnableOption "teams config";

  config = mkIf cfg.enable { home.packages = with pkgs; [ teams ]; };
}
