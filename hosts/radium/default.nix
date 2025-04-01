{ pkgs, config, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/mayrf
    ../common/users/primary
    ./vpn-kit.nix
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  mymodules.docker.enable = true;
  privModules.workProxies.enable = true;


  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
  # system.stateVersion = "24.11"; # Did you read the comment?

}
