{ config, pkgs, inputs, ... }: {

  imports = [
    ./sops.nix
    ./theming.nix
    ../../../modules/common/host-spec.nix
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
  services.gvfs.enable = true; #Belongs to gnome and nautilus, maybe try to turn off




  # Extra file
  environment = {
    variables = {
      TERMINAL = "alacritty";
      BROWSER = "librewolf";
      EDITOR = "vim";
      VISUAL = "nvim";
    };
  };




  networking = {
    hostName = config.hostSpec.hostName; # Define your hostname.
  };

  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Europe/Berlin";

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowUnfreePredicate = (_: true);
    };
  };
  security.polkit.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  services = {
    locate.enable = true;
    # Gnome Keyring, store keys for apps like nextcloud client
    gnome.gnome-keyring.enable = true;
  };

  security.rtkit.enable = true;
  xdg.portal.enable = true;
  xdg.portal.configPackages = [ pkgs.xdg-desktop-portal-gtk ];
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

  programs.dconf.enable = true;
}
