{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.postman;
in {
  options.features.desktop.postman.enable = mkEnableOption "postman config";

  config = mkIf cfg.enable {
    home.packages = with pkgs; [
      postman
      # needed for postman
      openssl
    ];
  };
}
