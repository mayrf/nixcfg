{ pkgs, config, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ../common/optional/ensure-config-repo.nix
    ../common/optional/keyd.nix
    ../common/optional/pipewire.nix
    ../common/optional/sops.nix
    ../common/optional/theming.nix
   # (import ./disko.nix { device = "/dev/sda"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "helium";
  };

  mymodules.docker.enable = true;
  mymodules.virtualisation.enable = true;
  mymodules.laptop.enable = true;
  # mymodules.impermanence.enable = true;

  sops.secrets."wireguard/x220_conf" = { };
  mymodules.vpn.enable = true;
  mymodules.vpn.configFile = config.sops.secrets."wireguard/x220_conf".path;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  system.stateVersion = config.hostSpec.sysStateVersion; # Did you read the comment?
}
