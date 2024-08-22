{ pkgs, host, pkgs-stable, lib, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix

    ../common/global
    ../common/users/mayrf
    ../common/optional/pipewire.nix
    ../common/optional/lutris.nix
    ../common/linux.nix
    ../common/optional/sddm.nix
    (import ./disko.nix { device = "/dev/nvme0n1"; })
  ];

  mymodules.docker.enable = true;
  mymodules.virtualisation = true;
  mymodules.gaming.enable = true;
  mymodules.impermanence.enable = true;

  nix.settings = {
    substituters = [ "https://hyprland.cachix.org" ];
    trusted-public-keys =
      [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
  };

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    # environmentVariables = { HSA_OVERRIDE_GFX_VERSION = "10.3.0"; };
    rocmOverrideGfx = "10.3.0";
    package = pkgs.ollama-rocm;
    # enable = false;
    enable = true;
    acceleration = "rocm";
  };

  networking = {
    hostName = host; # Define your hostname.
  };
  hardware.ledger.enable = true;

  system.stateVersion = "24.05"; # Did you read the comment?

}
