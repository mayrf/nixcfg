{  config, lib, ... }:
with lib;
let
  cfg = config.features.flatpak;
in {
  options.features.flatpak.enable =
    mkEnableOption "enable flatpak";
  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    xdg.portal.enable = true;
  };
}
