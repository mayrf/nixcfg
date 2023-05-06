
{ pkgs, ... }: 

{
  #imports = (import ./programs);
  #imports = [ ./progams/alacritty.nix ];
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
         normal = {
           family = "iMWritingMonoS Nerd Font"; 
           style = "Regular";
         };
         bold = {
           family = "iMWritingMonoS Nerd Font"; 
           style = "Bold";
         };
         italic = {
           family = "iMWritingMonoS Nerd Font"; 
           style = "Italic";
         };
         bold_italic = {
           family = "iMWritingMonoS Nerd Font"; 
           style = "Bold Italic";
         };
         size = 16;
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
      sxhkd = {
        enable = true;
        keybindings = {
          # Apps
          "super + Return" = "$TERMINAL";                       # Open Terminal
          "super + d" = "rofi -show drun";                  # Open Rofi (custom theme " -theme theme.rasi")
          "super + w" = "$BROWSER";                  # Open Browser 
          #"super + e" = "pcmanfm";                              # File Manager

          #"Print" = "flameshot gui";                            # Start flameshot gui

          # Bspwm
          "super + {q,k}" = "bspc node -{c,k}";                 # Close or Kill
          "super + alt + q" = "bspc quit";                       # Exit WM
          "super + r" = "bspc wm -r";                           # Reload WM
          "super + shift + r" = "pkill -usr1 -x sxhkd";                           # Reload WM

          # Super - Nodes
          "super + {_,shift +}{Left,Right,Up,Down}" = "bspc node -{f,s} {west,east,north,south}";  # Focus or move node in given direction
          "super + m" = "bspc desktop -l next";                 # Alternate between the tiled and monocle layout
          "super + {t,h,f}" = "bspc node -t '~{tiled,floating,fullscreen}'"; # Toggle between initial state and new state
            #"super + t" = "bspc node -t tiled";                   # Put node in tiled ( t is for tiled )
            #"super + h" = "bspc node -t floating";                # Put node in floating ( h is for hover )
            #"super + f" = "bspc node -t fullscreen";              # Toggle fullscreen ( f is for fullscreen )
          "super + g" = "bspc node -s biggest.window";          # Swap current node and the biggest window

          # Alt - Move workspaces
          "alt + {Left,Right}" = "bspc desktop -f {prev,next}.local"; # Focus the next/previous desktop in the current monitor
          "alt + {_,shift +}{1-9,0}" = "bspc {desktop -f,node -d} '{1-9,10}'";
          "alt + shift + {Left,Right}" = "bspc node -d {prev,next}.local --follow"; # Send and follow to previous or next desktop
            #"alt + {_,shift +}{ampersand,eacute,quotedbl,apostrophe,parenleft,section,egrave,exclam,ccedilla,agrave}" = "bspc {desktop -f,node -d} '{1-9,10}'"; # Focus or send to the given desktop for azerty


          # Control - Resize
          "control + {Left,Down,Up,Right}" = ''
            bspc node -z {left -20 0 || bspc node -z right -20 0, \
                          bottom 0 20 || bspc node -z top 0 20,\
                          top 0 -20 || bspc node -z bottom 0 -20,\
                          right 20 0 || bspc node -z left 20 0}
          '';                                                   # Expand and shrink
            #"control + {Left,Right,Up,Down}" = "bspc node -z {left -20 0,right 20 0,top 0 -20,bottom 0 20}";          # Expand window by moving one of its sides outwards
            #"control + shift + {Left,Right,Up,Down}" = "bspc node -z { right -20 0,left 20 0,bottom 0 -20,top 0 20}"; # Contract window by moving one of its sides inwards

          # XF86 Keys
          "XF86AudioMute" = "pactl list sinks | grep -q Mute:.no && pactl set-sink-mute 0 1 || pactl set-sink-mute 0 0";  # Toggle mute audio
          "XF86AudioRaiseVolume" = "pactl -- set-sink-volume 0 +10%";   # Raise volume
          "XF86AudioLowerVolume" = "pactl -- set-sink-volume 0 -10%";   # Lower volume
          "XF86AudioMicMute" = "pactl set-source-mute 1 toggle";        # Toggle mute mic audio
          "XF86MonBrightnessDown" = "light -U  5"; #"xrandr --output eDP-1 --brightness 0.3"; #"xbacklight -dec 10%";     # Brightness down
          "XF86MonBrightnessUp" = "light -A 5"; #"xrandr --output eDP-1 --brightness 1.0 "; #"xbacklight -inc 10%";       # Brightness up
        };
      };
    };
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
