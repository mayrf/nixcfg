{ pkgs, config, inputs, ... }:

{

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../common
    ../common/users
    inputs.dotfiles-private.outputs.nixosModules
    (import ./disko.nix { device = "/dev/sda"; })
  ];

  features = {
    ensure-config-repo.enable = true;
    keyd.enable = true;
    pipewire.enable = true;
    sops.enable = true;
    theming.enable = true;
  };

  hostSpec = {
    isMinimal = false;
    username = "mayrf";
    hostName = "helium";
  };
  features = {
    docker.enable = true;
    virtualisation.enable = true;
    laptop.enable = true;
    impermanence.enable = true;
  };

  sops.secrets."wireguard/x220_conf" = { };
  features.vpn = {
    enable = true;
    configFile = config.sops.secrets."wireguard/x220_conf".path;
  };

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?
}
