{ pkgs, config, inputs, ... }:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ./vpn-kit.nix
    inputs.dotfiles-private.outputs.nixosModules
  ];

  features = {
    sops.enable = true; 
    docker.enable = true;
    private = {
      workProxies.enable = true;
      work.enable = true;
    };
  };

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
}
