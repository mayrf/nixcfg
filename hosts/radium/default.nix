{ pkgs, config, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/primary
    ../common/users/primary/nixos.nix
    ./vpn-kit.nix
    ./proxy-vars.nix
  ];
  # privModules.workProxies.enable = true;

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  mymodules.docker.enable = true;


  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
}
