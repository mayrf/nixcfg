{ pkgs, host, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/mayrf
    ./vpn-kit.nix
  ];

  mymodules.docker.enable = true;
  privModules.workProxies.enable = true;

  networking = {
    hostName = host; # Define your hostname.
  };

  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
  system.stateVersion = "24.11"; # Did you read the comment?

}
