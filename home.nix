{ pkgs, ... }:

{
  imports = [
    # ./modules/programs
    ./modules/programs/lf
    ./modules/programs/alacritty.nix
    ./modules/services/sxhkd.nix
    ./modules/services/bspwm.nix
    ./modules/services/polybar
    ./modules/hyprland/home.nix
  ];
  systemd.user.services.polybar.Install.WantedBy =
    [ "graphical-session.target" ];
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [
    rsync # Syncer - $ rsync -r dir1/ dir2/
    unzip # Zip Files
    #unrar # Rar Files
    zip # Zip
    feh # Image Viewer
    sxiv
    nextcloud-client
    gnome.nautilus
    gnome.sushi
    onlyoffice-bin
    git
    git-crypt
    gnupg
    pinentry_qt
    htop
    freetube
    keepassxc
    signal-desktop
    tree
    vlc
    calibre
    libreoffice-fresh
    jdk17
    (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "iA-Writer" ]; })
    anki-bin
    tipp10
    ipscan
    trash-cli
    borgbackup
    borgmatic
    hugo
    sshfs

    # lf dependencies
    ctpv
    ueberzugpp
    ffmpeg_6-full
    jq
    poppler_utils
    bat
  ];
  home.stateVersion = "22.11";
  services.emacs.client.enable = true;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs = {
    starship = {
      enableZshIntegration = true;
      enable = true;
    };
    wofi = { enable = true; };
    zsh = {
      enable = true;

      shellAliases = {
        "rbs" = "sudo nixos-rebuild switch --flake $HOME/nixcfg/.#$MACHINE";

      };
      autocd = true;
      historySubstringSearch.enable = true;
      history = {
        ignoreDups = true;
        size = 100000;
      };
      initExtra = ''
        eval "$(direnv hook zsh)"
      '';
    };
    foot = {
      enable = true;
      server.enable = true;
    };
    gpg = { enable = true; };
    git = {
      enable = true;
      userName = "mayrf";
      userEmail = "70516376+mayrf@users.noreply.github.com";
    };
    neovim = {
      enable = true;
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
      extraConfig = ''
        set clipboard+=unnamedplus
        set number relativenumber
      '';
    };
    librewolf.enable = true;
  };

  services = {
    dunst = { enable = true; };
    nextcloud-client = {
      enable = true;
      startInBackground = true;
    };
    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
  };
}
