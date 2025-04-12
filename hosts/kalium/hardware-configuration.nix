{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot = {
    initrd = {
      availableKernelModules = [
        "ehci_pci"
        "ahci"
        "xhci_pci"
        "sd_mod"
        "sdhci_pci"
        "usbhid"
        "sd_mod"
        "uas"
      ];

      kernelModules = [ ];
    };
    kernelModules = [ "kvm-intel" ];
    loader.grub = {
      enable = true;
      configurationLimit = 10;
    };
  };

  # boot.loader = {
  #   systemd-boot = {
  #     configurationLimit = 10;
  #     enable = true;
  #   };
  # };

  # swapDevices = [{
  #   device = "/var/lib/swapfile";
  #   size = 16 * 1024;
  # }];
  services.fstrim.enable = true;

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}

