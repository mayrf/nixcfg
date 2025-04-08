{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.terminal.kitty;
in {
  options.features.terminal.kitty.enable = mkEnableOption "kitty config";

  config = mkIf cfg.enable { programs.kitty = { enable = true; }; };
}
