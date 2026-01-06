{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.cli.leetcode;
in {
  options.features.cli.leetcode.enable = mkEnableOption "enable leetcode cli program";
  config = mkIf cfg.enable {
    home.file.".leetcode".source = config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/leetcode";
    features.impermanence.directories = [ ".config/leetcode" ".config/leetgo" ];
    home.packages = with pkgs; [
      leetcode-cli
      leetgo
    ];
  };
}
