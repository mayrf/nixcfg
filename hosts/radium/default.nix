{ pkgs, host, ... }:

{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common/global
    ../common/users/mayrf
    ../common/optional/pipewire.nix
  ];

  mymodules.docker.enable = true;

  networking = {
    hostName = host; # Define your hostname.
  };

  environment.systemPackages = with pkgs; [ wsl-vpnkit ];

  systemd.services.wsl-vpnkit = {
    enable = true;
    description = "wsl-vpnkit";
    after = [ "network.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.wsl-vpnkit}/bin/wsl-vpnkit";
      Restart = "always";
      KillMode = "mixed";
    };
  };

  system.stateVersion = "24.11"; # Did you read the comment?
}
