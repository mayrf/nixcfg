{ pkgs, config, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ../common/optional/ensure-config-repo.nix
    ../common/optional/sops.nix
    ./vpn-kit.nix
    # ./proxy-vars.nix
  ];
  privModules.workProxies.enable = true;

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  features.docker.enable = true;


  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
}
