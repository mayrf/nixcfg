{ pkgs, config, inputs, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ./vpn-kit.nix
    inputs.dotfiles-private.outputs.nixosModules
    # ./proxy-vars.nix
  ];
  privModules.workProxies.enable = true;

  features = {
    ensure-config-repo.enable = true; 
    sops.enable = true; 
  };

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  features.docker.enable = true;


  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
}
