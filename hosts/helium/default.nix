{ pkgs, host, config, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/mayrf
    ../common/optional/pipewire.nix
    ../common/linux.nix
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  mymodules.docker.enable = true;
  mymodules.virtualisation.enable = true;
  mymodules.laptop.enable = true;
  # mymodules.impermanence.enable = true;

  sops.secrets."wireguard/x220_conf" = { };
  mymodules.vpn.enable = true;
  mymodules.vpn.configFile = config.sops.secrets."wireguard/x220_conf".path;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  networking = {
    hostName = host; # Define your hostname.
  };

  system.stateVersion = "24.11"; # Did you read the comment?
}
