{ config, pkgs, ... }: {
  imports = [ optional/sddm.nix optional/keyd.nix ];

  boot.supportedFilesystems = [ "ntfs" ];

  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      # Not sure if I need all these...
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  # Select internationalisation properties.
  console = {
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers =
    [ pkgs.gutenprint pkgs.brlaser pkgs.brgenml1lpr pkgs.brgenml1cupswrapper ];

  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = { Enable = "Source,Sink,Media,Socket"; };
  };
  virtualisation.spiceUSBRedirection.enable = true;
  programs.virt-manager.enable = true;
  virtualisation.libvirtd = {
    enable = true;

    onShutdown = "suspend";
    onBoot = "ignore";

    qemu = {
      package = pkgs.qemu_kvm;
      ovmf.enable = true;
      ovmf.packages = [ pkgs.OVMFFull.fd ];
      swtpm.enable = true;
      runAsRoot = false;
    };
  };

  environment.etc = {
    "ovmf/edk2-x86_64-secure-code.fd" = {
      source = config.virtualisation.libvirtd.qemu.package
        + "/share/qemu/edk2-x86_64-secure-code.fd";
    };

    "ovmf/edk2-i386-vars.fd" = {
      source = config.virtualisation.libvirtd.qemu.package
        + "/share/qemu/edk2-i386-vars.fd";
    };
  };

}
