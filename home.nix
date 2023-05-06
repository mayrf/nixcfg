
{ pkgs, ... }: 

{
  imports = [ 
    ./modules/programs/alacritty.nix  
    ./modules/services/sxhkd.nix
  ];
  systemd.user.services.polybar.Install.WantedBy = [ "graphical-session.target" ];	
  fonts.fontconfig.enable = true;
  home.packages = with pkgs; [ 
    git
    git-crypt
    gnupg
    pinentry_qt
    htop 
    freetube
    keepassxc
    tree
    (pkgs.nerdfonts.override { fonts = [ "FiraCode" "iA-Writer" ]; })
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
   alacritty = {
     enable = true;
     settings = {
       window = {
         padding = {
           x = 15;
           y = 15;
         }; 
       };
       scrolling = {
         history = 10000;
         multiplier = 3;
       };
       font = {
        #normal = {
         #  family = "Fira Mono"; 
          # style = "Regular";
        # };
         #bold = {
          # family = "iMWritingDuoS Nerd Font"; 
           #style = "Bold";
        # };
         #italic = {
          # family = "iMWritingDuoS Nerd Font"; 
           #style = "Italic";
         #};
         #bold_italic = {
         #  family = "iMWritingDuoS Nerd Font"; 
          # style = "Bold Italic";
        # };
         size = 9;
       };
       key_bindings = [ 
         { 
           key = "Return"; 
           mods = "Alt|Shift"; 
           action = "SpawnNewInstance";
         }
       ];
       colors = {
         # Default colors
         primary = {
           background = "#24273A"; # base
           foreground =  "#CAD3F5"; # text
           # Bright and dim foreground colors
           dim_foreground = "#CAD3F5"; # text
           bright_foreground = "#CAD3F5"; # text
         };
         # Cursor colors
         cursor = {
           text = "#24273A"; # base
           cursor = "#F4DBD6"; # rosewater
         };
         vi_mode_cursor = {
           text = "#24273A"; # base
           cursor = "#B7BDF8"; # lavender
         };

         # Search colors
         search= {
           matches= {
             foreground= "#24273A"; # base
             background= "#A5ADCB"; # subtext0
           };
           focused_match= {
             foreground= "#24273A"; # base
             background= "#A6DA95"; # green
           };
           footer_bar = {
             foreground = "#24273A"; # base
             background = "#A5ADCB"; # subtext0
           };
         };

         # Keyboard regex hints
         hints= {
           start= {
             foreground= "#24273A"; # base
             background= "#EED49F"; # yellow
           };
           end= {
             foreground= "#24273A"; # base
             background= "#A5ADCB"; # subtext0
           };
         };

         # Selection colors
         selection= {
           text= "#24273A"; # base
           background= "#F4DBD6"; # rosewater
         };
         # Normal colors
         normal = {
           black = "#494D64"; # surface1
           red= "#ED8796"; # red
           green= "#A6DA95"; # green
           yellow= "#EED49F"; # yellow
           blue= "#8AADF4"; # blue
           magenta= "#F5BDE6"; # pink
           cyan= "#8BD5CA"; # teal
           white= "#B8C0E0"; # subtext1
         };
         # Bright colors
         bright= {
           black= "#5B6078"; # surface2
           red= "#ED8796"; # red
           green= "#A6DA95"; # green
           yellow= "#EED49F"; # yellow
           blue= "#8AADF4"; # blue
           magenta= "#F5BDE6"; # pink
           cyan= "#8BD5CA"; # teal
           white= "#A5ADCB"; # subtext0
         };
	 # Dim colors
	 dim= {
           black= "#494D64"; # surface1
	   red= "#ED8796";# red
	   green= "#A6DA95"; # green
	   yellow= "#EED49F";# yellow
	   blue= "#8AADF4"; # blue
	   magenta= "#F5BDE6"; # pink
	   cyan= "#8BD5CA"; # teal
	   white= "#B8C0E0"; # subtext1
         };

         indexed_colors = [
           { index= 16; color= "#F5A97F"; }
           { index= 17; color= "#F4DBD6"; }
         ];
       };
      };
    };
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
    polybar = {	
      enable = true;  
      script = "polybar &";
      config = ./config/polybar/config;
    };
  };
  
}
