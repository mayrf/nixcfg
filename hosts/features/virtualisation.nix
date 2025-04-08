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

      qemu = {
        package = pkgs.qemu_kvm;
        ovmf.enable = true;
        ovmf.packages = [ pkgs.OVMFFull.fd ];
        swtpm.enable = true;
        runAsRoot = false;
      };
    };

    environment.etc = {
      "ovmf/edk2-x86_64-secure-code.fd" = {
        source = config.virtualisation.libvirtd.qemu.package
          + "/share/qemu/edk2-x86_64-secure-code.fd";
      };

      "ovmf/edk2-i386-vars.fd" = {
        source = config.virtualisation.libvirtd.qemu.package
          + "/share/qemu/edk2-i386-vars.fd";
      };
    };
  };
}
