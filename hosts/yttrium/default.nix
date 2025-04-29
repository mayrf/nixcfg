{ pkgs, config, inputs, ... }: {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    inputs.dotfiles-private.outputs.nixosModules
    (import ./disko.nix { device = "/dev/nvme0n1"; })
  ];

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "yttrium";
    persistDir = "/persist";
    isImpermanent = true;
    sysStateVersion = "25.05";
  };

  features = {
    flatpak.enable = true;
    keyd.enable = true;
    pipewire.enable = true;
    sops.enable = true;
    theming.enable = true;
    printing.enable = true;
    docker.enable = true;
    open-webui.enable = true;
    virtualisation.enable = true;
    gaming.enable = true;
    impermanence.enable = true;
    winapps.enable = true;
    private = {
      common.enable = true;
      vpn.enable = true;
    };
  };

  services.ollama = {
    rocmOverrideGfx = "10.3.0";
    package = pkgs.stable.ollama-rocm;
    enable = true;
    acceleration = "rocm";
  };

  environment.systemPackages = [ pkgs.nfs-utils ];

  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?

}
