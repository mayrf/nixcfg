{ pkgs, config, inputs, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    ../features
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
    devbox.enable = true;
  };

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "radium";
  };

  programs.nix-ld.enable = true;
  programs.nix-ld.libraries = with pkgs; [ chromium libglibutil ];
  environment.systemPackages = with pkgs; [ wsl-vpnkit ];
}
