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
      # Get disk is via: lsblk -o ID-LINK,PATH
      device =
        "/dev/disk/by-id/wwn-0x5001b444a7c374b6"; # or "nodev" for efi only
    };
  };

  # fileSystems."/" = {
  #   device = "/dev/disk/by-label/nixos";
  #   fsType = "ext4";
  # };

  # boot.initrd.luks.devices."crypt".device =
  #   "/dev/disk/by-uuid/63269954-250b-4521-8e83-9edb0a0233f2";

  # fileSystems."/boot" = {
  #   #      device = "/dev/disk/by-uuid/BFBC-D8CE";
  #   device = "/dev/disk/by-label/boot";
  #   fsType = "vfat";
  # };

  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 16 * 1024;
  }];
  services.fstrim.enable = true;

  networking.useDHCP = lib.mkDefault true;
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}

