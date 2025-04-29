{ pkgs, config, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "kalium";
  };


  features = {
    gaming.enable = true;
  };

  features.laptop.enable = true;

  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?
}
