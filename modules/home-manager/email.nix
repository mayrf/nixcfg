{ config, pkgs, stable, lib, ... }:
with lib;
let cfg = config.email;
in {
  options.email = { enable = mkEnableOption "my email config"; };
  config = mkIf cfg.enable {

    home.packages = with pkgs; [ oauth2ms isync cyrus-sasl-xoauth2 ];
    xdg.configFile = { "isyncrs".source = ./isyncrc; };
  };
}
