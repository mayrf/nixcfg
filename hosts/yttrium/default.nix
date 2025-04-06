{ pkgs, config, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ../common/optional/lutris.nix
    ../common/optional/ensure-config-repo.nix
    ../common/optional/keyd.nix
    ../common/optional/pipewire.nix
    ../common/optional/sops.nix
    ../common/optional/theming.nix
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
    sysStateVersion = "25.05";
  };

  mymodules.docker.enable = true;
  mymodules.open-webui.enable = true;
  mymodules.virtualisation.enable = true;
  mymodules.gaming.enable = true;
  mymodules.impermanence.enable = true;
  privModules.common.enable = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    rocmOverrideGfx = "10.3.0";
    package = pkgs.stable.ollama-rocm;
    enable = true;
    acceleration = "rocm";
  };

  system.stateVersion = config.hostSpec.sysStateVersion; # Did you read the comment?
}
