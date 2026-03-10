{
  config,
  pkgs,
  lib,
  ...
}:
with lib;
let
  cfg = config.features.bluetooth;
in
{
  options.features.bluetooth.enable = mkEnableOption "bluetooth config";
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      bluetuith
    ];
    services.blueman.enable = true;
    hardware.bluetooth.enable = true;
    hardware.bluetooth.settings = {
      General = {
        Enable = "Source,Sink,Media,Socket";
        Experimental = true; # enables battery reporting
        FastConnectable = true; # faster reconnection
      };
      Policy.AutoEnable = true;
    };
    features.impermanence.directories = [
      "/var/lib/bluetooth"
    ];
  };
}
