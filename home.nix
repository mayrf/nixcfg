
{ pkgs, ... }: 

{
  imports = [ 
    ./modules/programs/alacritty.nix  
    ./modules/services/sxhkd.nix
    ./modules/services/bspwm.nix
    ./modules/services/polybar
  ];
  systemd.user.services.polybar.Install.WantedBy = [ "graphical-session.target" ];	
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [ 
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
    ipscan
    (pkgs.nerdfonts.override { fonts = [ "JetBrainsMono" "iA-Writer" ]; })
  ];
  home.stateVersion = "22.11";
  programs = {
    gpg = {
      enable = true;  
    };
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
  #
# Keyboard shortcuts
#

  services = {
    dunst = {
      enable = true;
    };
    nextcloud-client = {
      enable = false;
      startInBackground = false;
    };
    gpg-agent = {
      enable = true;
      pinentryFlavor = "qt";
    };
    #polybar = {	
    #  enable = true;  
    #  script = "polybar &";
    #  config = ./config/polybar/config;
    #};
  };
  
}
