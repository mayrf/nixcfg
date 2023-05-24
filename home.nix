{ pkgs, ... }:

{
  imports = [
    ./modules/programs/alacritty.nix
    ./modules/services/sxhkd.nix
    ./modules/services/bspwm.nix
    ./modules/services/polybar
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
    borgbackup
    borgmatic
  ];
  home.stateVersion = "22.11";
  services.emacs.client.enable = true;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;
  programs = {
    lf = {
      enable = true;
      settings = {
        color256 = true;
        drawbox = true;
        hidden = true;
        ignorecase = true;
        number = true;
        preview = true;
        ratios = "2:4:3";
        relativenumber = true;
        scrolloff = 10;
        dirfirst = true;
        # sortby = "";
      };
    };
    zsh = {
      enable = true;

      shellAliases = {
        "rbs" = "sudo nixos-rebuild switch --flake $HOME/nixcfg/.#$MACHINE";
      };
      initExtra = ''
        PROMPT="%F{cyan}[%f%n%F{red}@%f%M%F{cyan}]%f  %B%F{84}%~%f%b "

        eval "$(direnv hook zsh)"
      '';
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
