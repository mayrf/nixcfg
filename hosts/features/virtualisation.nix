{ config, pkgs, lib, ... }:
with lib;
let cfg = config.features.virtualisation;
in {
  options.features.virtualisation = {
    enable = mkEnableOption "my virtualisation machine config";
  };
  config = mkIf cfg.enable {
    virtualisation.spiceUSBRedirection.enable = true;
    programs.virt-manager.enable = true;
    virtualisation.libvirtd = {
      enable = true;

      onShutdown = "suspend";
      onBoot = "ignore";

    };
  };
}
