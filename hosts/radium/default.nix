{
  pkgs,
  config,
  inputs,
  lib,
  ...
}:
{
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ../features
    # ./vpn-kit.nix
    inputs.dotfiles-private.outputs.nixosModules
  ];

  features = {
    private = {
      workProxies.enable = true;
      work.enable = true;
    };
    # devbox.enable = true;
  };

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [
    chromium
    libglibutil
  ];
  environment.systemPackages = with pkgs; [
    wsl-vpnkit
    xorg.xmodmap
  ];
  networking.networkmanager.enable = lib.mkForce false;
  # Optional: Create a systemd service to keep it running
  systemd.user.services.ssh-proxy = {
    description = "SSH SOCKS Proxy";
    after = [ "network.target" ];
    wantedBy = [ "default.target" ];

    serviceConfig = {
      ExecStart = "${pkgs.openssh}/bin/ssh -N -D 1080 hollama";
      Restart = "on-failure";
      RestartSec = "5s";
    };
  };
  services.dnsmasq = {
    enable = true;
    settings = {
      # Your custom host mappings
      address = [
        "/accounts.hobex.io/217.196.147.21"
      ];
      # Only listen locally
      listen-address = "127.0.0.1";
      bind-interfaces = true;
    };
  };
}
