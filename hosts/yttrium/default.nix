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
    # flakeDir = "/etc/nixos";
    persistDir = "/persist/system";
    persistDirRoot = "/persist";
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
  };

  privModules.common.enable = true;

  boot = {
    kernelPackages = pkgs.linuxKernel.packages.linux_6_14;
    binfmt.emulatedSystems = [ "aarch64-linux" "i686-linux" ];
  };

  services.ollama = {
    rocmOverrideGfx = "10.3.0";
    package = pkgs.stable.ollama-rocm;
    enable = true;
    acceleration = "rocm";
  };

  environment.systemPackages = with pkgs; [ nfs-utils ];
  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?
}
