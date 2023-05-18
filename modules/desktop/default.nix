{ config, pkgs, ... }:

let user = "mayrf";
in {
  imports = [ # Include the results of the hardware scan.
    ../editors/emacs/doom-emacs
    ../services/keyboard
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

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    font = "Lat2-Terminus16";
    # keyMap = "us";
    useXkbConfig = true; # use xkbOptions in tty.
  };

  services = {
    blueman.enable = true;
    picom = {
      enable = true;
      settings = { corner-radius = 10; };
    };
    xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "compose:menu";
      autoRepeatDelay = 250;
      autoRepeatInterval = 1000 / 60;
      enable = true;
      displayManager = {
        lightdm.enable = true;
        defaultSession = "none+bspwm";
      };
      windowManager.bspwm.enable = true;
      desktopManager.xfce.enable = true;
    };
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

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
