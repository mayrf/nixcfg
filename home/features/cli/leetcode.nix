{ config, pkgs, ... }:
{
  home.file.".leetcode".source = config.lib.file.mkOutOfStoreSymlink "${config.xdg.configHome}/leetcode";
  features.impermanence.directories = [ ".config/leetcode" ".config/leetgo" ];
  home.packages = with pkgs; [
    leetcode-cli
    leetgo
  ];
}
