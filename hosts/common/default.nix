{ config, pkgs, inputs, outputs, ... }: {

  imports = [
    ../../modules/common/host-spec.nix
    ../features
    ./nixos-cli.nix
    inputs.home-manager.nixosModules.home-manager
  ];

  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.
  hardware.graphics = {
    enable = true;
    # extraPackages = with pkgs; [
    #   # Not sure if I need all these...
    #   intel-media-driver # LIBVA_DRIVER_NAME=iHD
    #   vaapiIntel # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
    #   vaapiVdpau
    #   libvdpau-va-gl
    # ];
  };

  # No matter what environment we are in we want these tools for root, and the user(s)
  programs.git.enable = true;
  programs.zsh.enable = true;

  # Select internationalisation properties.
  console = {
    useXkbConfig = true; # use xkbOptions in tty.
  };

  # optional
  # services.udev.packages = [ pkgs.yubikey-personalization ];

  #gnome stuff
  programs.dconf.enable = true;
  # needed for gnome keyring / docker credentials
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = false;

  # Extra file
  environment = {
    variables = {
      TERMINAL = "ghostty";
      BROWSER = "librewolf";
      EDITOR = "vim";
      VISUAL = "nvim";
    };
  };

  security.polkit.enable = true;

  security.pam.services.hyprlock = { }; # new line

  security.rtkit.enable = true;

  networking = {
    hostName = config.hostSpec.hostName; # Define your hostname.
  };

  # boot.kernelPackages = pkgs.linuxKernel.kernels.linux_6_14;
  boot.kernelPackages = pkgs.unstable.linuxPackages_zen;

  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Europe/Berlin";
  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  services = {
    locate.enable = true;
    # Gnome Keyring, store keys for apps like nextcloud client
    gnome.gnome-keyring.enable = true;
  };
  services.gvfs.enable =
    true; # Belongs to gnome and nautilus, maybe try to turn off

  environment.systemPackages = with pkgs; [

    file
    urlencode
    nmap
    mlocate
    openssl
    tldr
    unzip # Zip Files
    htop
    trash-cli
    jq
    yq
    tree
    zip # Zip
    rsync # Syncer - $ rsync -r dir1/ dir2/
    fd
    busybox
    bluetuith
    vim
    wget
    libsForQt5.qt5.qtwayland
    libsForQt5.polkit-kde-agent
    pass
    gnupg
    #Utils

    borgbackup
    borgmatic
    sshfs
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

  nix = {
    nixPath = [ "nixpkgs=${inputs.nixpkgs}" ];
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      keep-outputs = true;
      # allowed-users = [ "root" "${config.hostSpec.username}" ];
      trusted-users = [ "root" "${config.hostSpec.username}" ];
      substituters =
        [ "https://hyprland.cachix.org" "https://nix-community.cachix.org/" ];
      trusted-public-keys = [
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };

}
