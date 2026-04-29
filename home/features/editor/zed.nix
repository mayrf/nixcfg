{ config, pkgs, ... }:
{
  features.impermanence.directories = [
    ".local/share/zed"
    ".config/zed"
  ];
  programs.zed-editor = {
    enable = true;
    package = pkgs.stable.zed-editor-fhs;
    extensions = [ "nix" "toml" "rust" ];
    userSettings = {
      hour_format = "hour24";
      vim_mode = true;
    };
  };
}
