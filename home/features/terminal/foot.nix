{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.terminal.foot;
in {
  options.features.terminal.foot.enable = mkEnableOption "foot config";

  config = mkIf cfg.enable {
    programs = {
      foot = {
        enable = true;
        server.enable = true;
        settings = { mouse = { hide-when-typing = "yes"; }; };
      };
    };
  };
}
