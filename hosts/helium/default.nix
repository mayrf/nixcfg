{
  pkgs,
  config,
  inputs,
  ...
}:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./distributed-builds.nix
    ../common
    ../common/users
    ../features
    ../features/desktop
    inputs.dotfiles-private.outputs.nixosModules
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x220
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  features = {
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
  };

  system.stateVersion = config.hostSpec.sysStateVersion; # Did you read the comment?
}
