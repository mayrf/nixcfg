{ config, pkgs, inputs, outputs, ... }: {

    

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

  # No matter what environment we are in we want these tools for root, and the user(s)
  programs.git.enable = true;
  programs.zsh.enable = true;

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

  imports = [
    # ../../modules/common/host-spec.nix
    # ./theming.nix
    # ./ensure-config-repo.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  # optional
  services.flatpak.enable = true;
  # services.udev.packages = [ pkgs.yubikey-personalization ];

  services.blueman.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };



  #gnome stuff
  services.gvfs.enable = true; #Belongs to gnome and nautilus, maybe try to turn off
  programs.dconf.enable = true;
  # needed for gnome keyring / docker credentials
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;
  security.pam.services = { swaylock = { }; };



  # Extra file
  environment = {
    variables = {
      TERMINAL = "alacritty";
      BROWSER = "librewolf";
      EDITOR = "vim";
      VISUAL = "nvim";
    };
  };





  security.polkit.enable = true;


  security.rtkit.enable = true;
  xdg.portal.enable = true;
  xdg.portal.configPackages = [ pkgs.xdg-desktop-portal-gtk ];

  networking = {
    hostName = config.hostSpec.hostName; # Define your hostname.
  };

  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Europe/Berlin";
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  services = {
    locate.enable = true;
    # Gnome Keyring, store keys for apps like nextcloud client
    gnome.gnome-keyring.enable = true;
  };

  environment.systemPackages = with pkgs; [
    fd
    busybox
    bluetuith
    vim
    wireguard-tools
    nixfmt-classic
    wget
    system-config-printer
    riseup-vpn
    libsForQt5.qt5.qtwayland
    # pkgs-stable.rustdesk
    libsForQt5.polkit-kde-agent
    pass
    gnupg
  ];

  fonts.packages = with pkgs; [
    vistafonts
    noto-fonts
    noto-fonts-cjk-sans
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts.githubRelease
    dina-font
    proggyfonts
    roboto
  ];

  nixpkgs = {
    overlays = [
      inputs.emacs-overlay.overlays.emacs
      outputs.overlays.additions
      outputs.overlays.stable-packages
      outputs.overlays.unstable-packages
    ];
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };
  nix.optimise.automatic = true;
  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      keep-outputs = true;
      allowed-users = [ "${config.hostSpec.username}" ];
      substituters =
        [ "https://hyprland.cachix.org" "https://nix-community.cachix.org/" ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

}
