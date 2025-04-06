{ pkgs, lib, stable, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/primary
    ../common/users/primary/nixos.nix
    ../common/optional/pipewire.nix
    ../common/optional/lutris.nix
    ../common/linux.nix
    ../common/optional/sddm.nix
    (import ./disko.nix { device = "/dev/nvme0n1"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "yttrium";
    flakeDir = "/etc/nixos";
    persistDir = "/persist/system";
    persistDirRoot = "/persist";
    isImpermanent = true;
  };

  mymodules.docker.enable = true;
  mymodules.open-webui.enable = true;
  mymodules.virtualisation.enable = true;
  mymodules.gaming.enable = true;
  mymodules.impermanence.enable = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    rocmOverrideGfx = "10.3.0";
    package = stable.ollama-rocm;
    enable = true;
    acceleration = "rocm";
  };

  hardware.ledger.enable = true;

  system.stateVersion = "25.05"; # Did you read the comment?

}
