{ pkgs, config, inputs, stable, ... }: {
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
    kanata.enable = true;
    pipewire.enable = true;
    sops.enable = true;
    theming.enable = true;
    printing.enable = true;
    bluetooth.enable = true;
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
    environmentVariables = {
      OLLAMA_CONTEXT_LENGTH = "8192"; 
    };
  };
  features.impermanence.directories = [
    "/var/lib/private/open-webui"
    {
      directory = "/var/lib/private";
      mode = "u=rwx,g=,o=";
    }
    {
      directory = "/var/lib/private/ollama";
      mode = "0700";
    }
  ];

  environment.systemPackages = [
    pkgs.nfs-utils
    # pkgs.brscan4
    # pkgs.stable.brscan4
  ];
  hardware.sane = {
    enable = true;
    brscan4 = {
      enable = true;
      netDevices = {
        office1 = {
          ip = "192.168.0.109";
          model = "MFC-L2800DW";
        };
      };
    };
  };

  system.stateVersion =
    config.hostSpec.sysStateVersion; # Did you read the comment?

}
