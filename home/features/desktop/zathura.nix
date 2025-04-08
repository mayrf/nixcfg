{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.zathura;
in {
  options.features.desktop.zathura.enable = mkEnableOption "zathura config";

  config = mkIf cfg.enable {
    programs.zathura = {
      enable = true;
      options = { "selection-clipboard" = "clipboard"; };
    };
  };
}
