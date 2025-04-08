{ config, lib, pkgs, ... }:
with lib;
let cfg = config.features.desktop.virtualisation;
in {
  options.features.desktop.virtualisation.enable =
    mkEnableOption "virtualisation config";

  config = mkIf cfg.enable {
    # TODO Add qemu, etc.
    # For virtualisation
    dconf.settings = {
      "org/virt-manager/virt-manager/connections" = {
        autoconnect = [ "qemu:///system" ];
        uris = [ "qemu:///system" ];
      };
    };
  };
}
