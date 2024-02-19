{ config, pkgs, user, host, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../common/global
    ../common/users/mayrf
    ../common/optional/docker.nix
    ../common/optional/pipewire.nix

  ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  networking = {
    hostName = host; # Define your hostname.
  };

  system.stateVersion = "23.11"; # Did you read the comment?
}
