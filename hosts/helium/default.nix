{ pkgs, config, inputs, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./distributed-builds.nix
    ../common
    ../common/users
    inputs.dotfiles-private.outputs.nixosModules
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x220
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  features = {
    flatpak.enable = true;
    keyd.enable = true;
    pipewire.enable = true;
    sops.enable = true;
    theming.enable = true;
    printing.enable = true;
    docker.enable = true;
    laptop.enable = true;
    bluetooth.enable = true;
    impermanence.enable = true;
    private = {
      common.enable = true;
      vpn.enable = true;
    };
  };


  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "helium";
    persistDir = "/persist";
    isImpermanent = true;
    sysStateVersion = "25.05";
  };

  sops.secrets."wireguard/x220_conf" = { };
  features.vpn = {
    enable = true;
    configFile = config.sops.secrets."wireguard/x220_conf".path;
  };

  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?
}
