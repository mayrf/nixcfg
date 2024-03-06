{ pkgs, host, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../common/global
    ../common/users/mayrf
    ../common/optional/docker.nix
    ../common/optional/pipewire.nix

  ];

  #boot = {
  #  kernelPackages = pkgs.linuxKernel.packages.linux_zen;
  #};

  networking = {
    hostName = host; # Define your hostname.
  };

  environment.systemPackages = with pkgs; [ wsl-vpnkit ];

  system.stateVersion = "23.11"; # Did you read the comment?
}
