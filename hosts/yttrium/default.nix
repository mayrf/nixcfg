{ pkgs, host, pkgs-master, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../common/global
    ../common/users/mayrf
    ../common/optional/docker.nix
    ../common/optional/pipewire.nix
    ../common/optional/lutris.nix
    ../common/linux.nix
  ];

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    environmentVariables = { HSA_OVERRIDE_GFX_VERSION = "10.3.0"; };
    package = pkgs-master.ollama;
    enable = true;
    acceleration = "rocm";
  };

  networking = {
    hostName = host; # Define your hostname.
  };
  hardware.ledger.enable = true;

  system.stateVersion = "23.11"; # Did you read the comment?
}
