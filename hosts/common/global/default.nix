{ config, pkgs, inputs, outputs, ... }: {

  imports = [
    ../../../modules/common/host-spec.nix
    ./theming.nix
    ./ensure-config-repo.nix
  ];

  # optional
  services.flatpak.enable = true;
  # services.udev.packages = [ pkgs.yubikey-personalization ];

  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  services.blueman.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };

  # services.openssh.enable = true;


  #gnome stuff
  services.gvfs.enable = true; #Belongs to gnome and nautilus, maybe try to turn off
  programs.dconf.enable = true;




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
