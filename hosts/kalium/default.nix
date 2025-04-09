{ pkgs, config, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "kalium";
  };

  # features.docker.enable = true;
  # features.virtualisation.enable = true;
  features.laptop.enable = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
  };


  system.stateVersion = config.hostSpec.sysStateVersion; # Did you read the comment?
}
