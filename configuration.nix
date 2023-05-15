# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

let user = "mayrf";
in {

  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./modules/editors/emacs/doom-emacs
  ];

  # Configure Swapfile.
  swapDevices = [{
    device = "/var/lib/swapfile";
    size = 16 * 1024;
  }];
  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    configurationLimit = 5;
    device = "/dev/sda"; # or "nodev" for efi only
  };

  security.sudo.wheelNeedsPassword = false;

  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive you want to install Grub.
  # boot.loader.grub.device = "/dev/vda"; # or "nodev" for efi only

  networking.hostName = "nixBox"; # Define your hostname.
  # Pick only one of the below networking options.
  #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  networking.networkmanager.enable =
    true; # Easiest to use and most distros use this by default.

  # Set your time zone.  #
  time.timeZone = "Europe/Berlin";

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
    keyd = {
      enable = true;
      settings = {
        main = { capslock = "overload(caps_layer, esc)"; };
        caps_layer = {
          j = "down";
          k = "up";
          h = "left";
          l = "right";
        };
        altgr = {
          a = "ä";
          q = "á";

          j = "ű";
          u = "ü";
          y = "ú";

          l = "ő";
          o = "ö";
          p = "ó";
        };
        "altgr+shift" = {
          A = "Ä";
          Q = "Á";

          J = "Ű";
          U = "Ü";
          Y = "Ú";

          L = "Ő";
          O = "Ö";
          P = "Ó";
        };
      };
    };
    blueman.enable = true;
    picom = {
      enable = true;
      settings = { corner-radius = 10; };
    };
    xserver = {
      layout = "us";
      xkbVariant = "altgr-intl";
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
  services.xserver.resolutions = [{
    x = 1920;
    y = 1080;
  }];

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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.zsh.enable = true;
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
}
