{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.editor.zed;
in {
  options.features.editor.zed.enable = mkEnableOption "my zed user config";
  config = mkIf cfg.enable {
    # features.impermanence.directories = [
    #   # ".config/Code"
    # ];
    # programs.nix-ld.enable = true;
    programs.zed-editor = {
      enable = true;
      extensions = [ "nix" "toml" "rust" ];
      userSettings = {
        theme = {
          mode = "system";
          dark = "One Dark";
          light = "One Light";
        };
        hour_format = "hour24";
        vim_mode = true;
      };
    };
    # home.packages = [
    #   pkgs.zed-editor.fhsWithPackages
    #   (pkgs: with pkgs; [ openssl zlib ])
    # ];
  };
}
