{ config, pkgs, user, hyprland, system, ... }: {
  imports = [ # Include the results of the hardware scan.
    ../hyprland
    ../editors/emacs/doom-emacs
    ../services/keyboard
    ../services/picom
    ../x11
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

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    # keyMap = "us";
    useXkbConfig = true; # use xkbOptions in tty.
  };
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
  # Configure keymap in X11

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.bluetooth.enable = true;
  hardware.bluetooth.settings = {
    General = { Enable = "Source,Sink,Media,Socket"; };
  };
  hardware.pulseaudio.package = pkgs.pulseaudioFull;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  programs.zsh.enable = true;
  users.defaultUserShell = pkgs.zsh;
  environment.shells = with pkgs; [ zsh ];
  users.users.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
    initialPassword = "password";
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    vim
    nixfmt
    wget
    firefox
    dmenu
    st
    rofi
    polybar
    xclip
  ];

  system.stateVersion = "22.11"; # Did you read the comment?
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
