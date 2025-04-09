{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.pipewire;
in {
  options.features.pipewire.enable = mkEnableOption "pipewire config";
  config = mkIf cfg.enable {
    security.rtkit.enable = true;
    services.pulseaudio.enable = false;
    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
    };
  };
}
