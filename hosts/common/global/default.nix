{ config, pkgs, pkgs-stable, catppuccin, ... }: {

  # imports = [ ./sops.nix ];

  security.sudo.wheelNeedsPassword = false;
  time.timeZone = "Europe/Berlin";
  services.udev.packages = [ pkgs.yubikey-personalization ];

  services.vscode-server.enable = true;
  services.vscode-server.enableFHS = true;
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };
  catppuccin = {
    enable = true;
    flavor = "macchiato"; # type: one of “latte”, “frappe”, “macchiato”, “mocha”
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
  services.openssh.enable = true;
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  services.gvfs.enable = true;

  security.rtkit.enable = true;
  xdg.portal.enable = true;
  xdg.portal.configPackages = [ pkgs.xdg-desktop-portal-gtk ];
  environment.systemPackages = with pkgs; [
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
    woeusb-ng
  ];
  fonts.packages = with pkgs; [
    vistafonts
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    mplus-outline-fonts.githubRelease
    dina-font
    proggyfonts
  ];

  nix.optimise.automatic = true;
  nix = {
    package = pkgs.nixFlakes;
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      keep-outputs = true;
    };
  };

  programs.dconf.enable = true;
}
