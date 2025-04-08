{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.gpg;
in {
  options.features.desktop.gpg.enable = mkEnableOption "gpg config";

  config = mkIf cfg.enable {
    programs.gpg = { enable = true; };
  };
}
