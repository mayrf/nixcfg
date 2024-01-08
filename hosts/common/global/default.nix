{ config, pkgs, inputs, user, ... }: {
  imports = [
    #   ./sops.nix
    ./keyd.nix
    ./sddm.nix
  ];

  boot.supportedFilesystems = [ "ntfs" ];
  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Europe/Berlin";

  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.
  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      # Not sure if I need all these...
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      vaapiVdpau
      libvdpau-va-gl
    ];
  };
  services.udev.packages = [ pkgs.yubikey-personalization ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
      permittedInsecurePackages = [ "electron-19.1.9" ];
    };
  };
  security.polkit.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    useXkbConfig = true; # use xkbOptions in tty.
  };
  services.flatpak.enable = true;
  services = {
    locate.enable = true;
    blueman.enable = true;
    # Gnome Keyring, store keys for apps like nextcloud client
    gnome.gnome-keyring.enable = true;
  };
  environment = {
    variables = {
      TERMINAL = "alacritty";
      BROWSER = "librewolf";
      EDIOR = "nvim";
      VISUAL = "nvim";
    };
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.gutenprint ];
  services.openssh.enable = true;
  services.avahi.enable = true;
  services.gvfs.enable = true;

  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = { Enable = "Source,Sink,Media,Socket"; };
  };
  security.rtkit.enable = true;
  virtualisation.libvirtd.enable = true;

  programs.dconf.enable = true;
  xdg.portal.enable = true;
  xdg.portal.extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  environment.systemPackages = with pkgs; [
    bluetuith
    vim
    wireguard-tools
    nixfmt
    wget
    system-config-printer
    riseup-vpn
    libsForQt5.qt5.qtwayland
    rustdesk
    etcher
    libsForQt5.polkit-kde-agent

  ];

  nix = {
    package = pkgs.nixFlakes;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      keep-outputs = true;
    };
  };
}
