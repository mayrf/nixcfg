{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.cli.scripts;
in {
  options.features.cli.scripts.enable = mkEnableOption "my user scripts config";
  config = mkIf cfg.enable {
    home.sessionPath = [
        "$HOME/.config/nixcfg/home/features/cli/scripts/bin"
    ];
    home.packages = with pkgs; [
    ];
  };
}
