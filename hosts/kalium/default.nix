{ pkgs, config, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/primary
    ../common/users/primary/nixos.nix
    ../common/optional/pipewire.nix
    ../common/linux.nix
   # (import ./disko.nix { device = "/dev/sda"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "kalium";
  };

  mymodules.docker.enable = true;
  mymodules.virtualisation.enable = true;
  mymodules.laptop.enable = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    # binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };


  system.stateVersion = "25.05"; # Did you read the comment?
}
