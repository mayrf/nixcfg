
{ config, pkgs, user, ... }:
let user = "mayrf";
in {

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../modules/editors/emacs/doom-emacs
    ../../modules/desktop
  ];

  # Configure Swapfile.
  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 1 * 1024;
  }];

  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    configurationLimit = 5;
    device = "/dev/vda"; # or "nodev" for efi only
  };
  networking.hostName = "nixBox"; # Define your hostname.

  environment = {
    variables = {
      MACHINE = "vm";
    };
  };

  services.xserver.resolutions = [{
    x = 1920;
    y = 1080;
  }];

}
