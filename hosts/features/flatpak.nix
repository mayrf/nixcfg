{ config, lib, ... }:
with lib;
let
  cfg = config.features.flatpak;
in
{
  options.features.flatpak.enable = mkEnableOption "enable flatpak";
  config = mkIf cfg.enable {
    services.flatpak.enable = true;
    xdg.portal.enable = true;
    xdg.portal.config.common.default = "*";
    persistence = {
      directories = [

      ];

    };
    # home/user/.cache/flatpak/system-cache/summaries/flathub.idx.sig
    # home/user/.cache/flatpak/system-cache/summaries/flathub.idx
    # home/user/.local/share/flatpak/.changed
    # home/user/.local/share/flatpak/repo/config
  };
}
