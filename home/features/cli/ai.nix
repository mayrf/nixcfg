{ config, lib, pkgs, hostSpec, ... }:
with lib;
let cfg = config.features.cli.ai;
in {
  options.features.cli.ai.enable = mkEnableOption "enable ai cli programs";

  config = mkIf cfg.enable {

    features.impermanence.directories =
      [ ".config/fabric" ".local/share/oterm" ];
    home.packages = with pkgs; [ oterm fabric-ai ];
  };
}
