{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    initrd = {
      availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "sd_mod" "sdhci_pci" "usbhid" "uas" ];

      kernelModules = [ ];
    };
    kernelModules = [ "kvm-intel" ];
    loader.grub = {
      enable = true;
      configurationLimit = 5;
      # Get disk is via: lsblk -o ID,PATH
      device = "/dev/disk/by-id/wwn-0x5001b444a7c374b6"; # or "nodev" for efi only
    };
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
  };

  boot.initrd.luks.devices."crypt".device = "/dev/disk/by-uuid/e16c06d8-9273-4478-93d3-dd6408b53fa6";


  fileSystems."/boot" = {
    device = "/dev/disk/by-label/boot";
    fsType = "vfat";
  };


  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 16 * 1024;
  }];

  networking.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
}
