{ config, lib, inputs, pkgs, ... }:
with lib;
let cfg = config.features.desktop.hyprland;
in {
  options.features.desktop.hyprland.enable = mkEnableOption "hyprland config";

  config = mkIf cfg.enable {

    xdg.portal.enable = true;
    xdg.portal.config.common.default = "*";

    fontProfiles = {
      enable = true;
      monospace = {
        family = "FiraCode Nerd Font";
        package = pkgs.unstable.nerd-fonts.fira-code;
      };
      regular = {
        family = "Fira Sans";
        package = pkgs.fira;
      };
    };
    programs = {
      fish.loginShellInit = ''
        if test (tty) = "/dev/tty1"
          exec Hyprland &> /dev/null
        end
      '';
      zsh.loginExtra = ''
        if [ "$(tty)" = "/dev/tty1" ]; then
          exec Hyprland &> /dev/null
        fi
      '';
      zsh.profileExtra = ''
        if [ "$(tty)" = "/dev/tty1" ]; then
          exec Hyprland &> /dev/null
        fi
      '';
    };

    xdg.configFile."xkb/rules/evdev".text = ''
      ! option = symbols
        hungarian_letters:huletters    = +hungarian_letters(huletters)
      ! include %S/evdev
    '';
    xdg.configFile."xkb/symbols/hungarian_letters".text = ''
      xkb_symbols "huletters" {
          //ä on alt+a
          key <AC01> { [     a,   A, adiaeresis,  Adiaeresis      ]   };
          //á on alt+q
          key <AD01> { [     q,   Q, aacute,      Aacute          ]   };

          //ü on alt+u
          key <AD07> { [     u,   U, udiaeresis,  Udiaeresis      ]   };
          //ü on alt+j
          key <AC07> { [     j,   J, udoubleacute,  Udoubleacute      ]   };
          //ú on alt+y
          key <AD06> { [     y,   Y, uacute,      Uacute          ]   };

          //ö on alt+o
          key <AD09> { [     o,   O, odiaeresis,  Odiaeresis      ]   };
          //ő on alt+p
          key <AD10> { [     p,   P, odoubleacute,  Odoubleacute      ]   };
          //ó on alt+l
          key <AC09> { [     l,   L, oacute,      Oacute      ]   };


          // make right alt altGr
          include "level3(ralt_switch)"
      };  '';
    home.packages = with pkgs; [ cliphist hyprpicker brightnessctl xorg.xhost ];
    programs.hyprlock.enable = true; # new line
    services.hypridle.enable = true;
    services.hypridle.settings = {
      general = {
        lock_cmd =
          "pidof hyprlock || hyprlock"; # avoid starting multiple hyprlock instances.
        before_sleep_cmd = "loginctl lock-session"; # lock before suspend.
        after_sleep_cmd =
          "hyprctl dispatch dpms on"; # to avoid having to press a key twice to turn on the display.
      };
      listener = [
        {
          timeout = 150; # 2.5min.
          on-timeout =
            "brightnessctl -s set 10"; # set monitor backlight to minimum, avoid 0 on OLED monitor.
          on-resume = "brightnessctl -r"; # monitor backlight restore.
        }
        {
          # timeout = 900;
          # timeout = 90;
          # on-timeout = "hyprlock";
          timeout = 600; # 10min
          on-timeout =
            "loginctl lock-session"; # lock screen when timeout has passed
        }
        {
          # timeout = 900; # 15min
          timeout = 180; # 3min
          on-timeout =
            "hyprctl dispatch dpms off"; # screen off when timeout has passed
          on-resume =
            "hyprctl dispatch dpms on && brightnessctl -r"; # screen on when activity is detected after timeout has fired.
        }
        {
          timeout = 1800; # 30min
          on-timeout = "systemctl suspend"; # suspend pc
        }
      ];
    };

    wayland.windowManager.hyprland = let
      workspaces = (map toString (lib.range 0 9))
        ++ (map (n: "F${toString n}") (lib.range 1 12));
      # Map keys to hyprland directions
      directions = rec {
        left = "l";
        right = "r";
        up = "u";
        down = "d";
        h = left;
        l = right;
        k = up;
        j = down;
      };
    in {
      # package =
      #   inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      # portalPackage =
      #   inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.xdg-desktop-portal-hyprland;
      enable = true;

      settings = {
        ecosystem."no_update_news" = true;
        bind = [
          "SUPER,mouse:272,movewindow"
          # "SUPER,mouse:273,resizewindow"

          "SUPERSHIFT,q,killactive"

          "SUPER,s,togglesplit"
          "SUPER,f,fullscreen,1"
          "SUPERSHIFT,f,fullscreen,0"
          "SUPERSHIFT,space,togglefloating"

          "SUPER,h,splitratio,-0.1"
          "SUPER,minus,splitratio,-0.25"
          "SUPERSHIFT,minus,splitratio,-0.3333333"

          "SUPER,l,splitratio,0.1"
          "SUPER,equal,splitratio,0.25"
          "SUPERSHIFT,equal,splitratio,0.3333333"

          "SUPER,g,togglegroup"
          "SUPER,apostrophe,changegroupactive,f"
          "SUPERSHIFT,apostrophe,changegroupactive,b"

          "SUPER,u,togglespecialworkspace"
          "SUPERSHIFT,u,movetoworkspace,special"

          "SUPERSHIFT,Backspace,exec, wofi-shutdown "
        ] ++
          # Change workspace
          (map (n: "SUPER,${n},workspace,name:${n}") workspaces) ++
          # Move window to workspace
          (map (n: "SUPERSHIFT,${n},movetoworkspacesilent,name:${n}")
            workspaces) ++
          # Move focus
          # (lib.mapAttrsToList (key: direction:
          #   "SUPER,${key},movefocus,${direction}"
          # ) directions) ++
          [
            "SUPER, Space, layoutmsg,swapwithmaster"
            "SUPER, J, layoutmsg, cyclenext"
            "SUPER, K, layoutmsg, cycleprev"
          ] ++
          # Swap windows
          (lib.mapAttrsToList
            (key: direction: "SUPERSHIFT,${key},swapwindow,${direction}")
            directions) ++
          # Move monitor focus
          (lib.mapAttrsToList
            (key: direction: "SUPERCONTROL,${key},focusmonitor,${direction}")
            directions) ++
          # Move window to other monitor
          (lib.mapAttrsToList (key: direction:
            "SUPERCONTROLSHIFT,${key},movewindow,mon:${direction}") directions)
          ++
          # Move workspace to other monitor
          (lib.mapAttrsToList (key: direction:
            "SUPERALT,${key},movecurrentworkspacetomonitor,${direction}")
            directions);
      };

      # This is order sensitive, so it has to come here.
      extraConfig = let
        swaylock = "${config.programs.swaylock.package}/bin/swaylock";
        playerctl = "${config.services.playerctld.package}/bin/playerctl";
        playerctld = "${config.services.playerctld.package}/bin/playerctld";
        # makoctl = "${config.services.mako.package}/bin/makoctl";
        wofi = "${config.programs.wofi.package}/bin/wofi";
        # pass-wofi = "${pkgs.pass-wofi.override {
        # pass = config.programs.password-store.package;
        # }}/bin/pass-wofi";

        grimblast = "${pkgs.grimblast}/bin/grimblast";
        pactl = "${pkgs.pulseaudio}/bin/pactl";

        gtk-launch = "${pkgs.gtk3}/bin/gtk-launch";
        xdg-mime = "${pkgs.xdg-utils}/bin/xdg-mime";
        defaultApp = type: "${gtk-launch} $(${xdg-mime} query default ${type})";

        # terminal = config.home.sessionVariables.TERMINAL;
        # browser = defaultApp "x-scheme-handler/https";
        # editor = defaultApp "text/plain";
        # terminal = "${pkgs.kitty}/bin/kitty";
        terminal = "${pkgs.unstable.ghostty}/bin/ghostty";
        terminal-exec = "${pkgs.unstable.ghostty}/bin/ghostty -e";
        browser = "${pkgs.librewolf}/bin/librewolf";
        filemanager = "${pkgs.yazi}/bin/yazi";

        brave = "${pkgs.brave}/bin/brave";
        editor = "${pkgs.emacs}/bin/emacsclient -c";
        vanilla_emacs = "${pkgs.emacs}/bin/emacsclient -s vanilla -c";
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";
        #   exec=${pkgs.swaybg}/bin/swaybg -i ${config.wallpaper} --mode fill
        # '' + ''
      in ''
        exec-once=wl-paste --type text --watch cliphist store # Stores only text data
        exec-once=wl-paste --type image --watch cliphist store # Stores only image data
        exec-once=${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1
        exec-once = hypridle
      '' + ''
        animations {
          enabled=false
        }

        decoration {
          blur {
            ignore_opacity=true
            new_optimizations=true
            passes=3
            size=5
          }
          active_opacity=1.000000
          # col.shadow=rgba(231e1899)
          fullscreen_opacity=1.000000
          inactive_opacity=0.840000
        }

        general {
          border_size=1
          col.active_border=rgb(88a4d3)
          col.inactive_border=rgb(9d8b70)
          gaps_in=1
          gaps_out=1
          layout=master
          # layout=dwindle
        }

        group {
          groupbar {
            col.active=rgb(88a4d3)
            col.inactive=rgb(9d8b70)
            text_color=rgb(cabcb1)
          }
          col.border_active=rgb(88a4d3)
          col.border_inactive=rgb(9d8b70)
          col.border_locked_active=rgb(6eb958)
        }

        input {
          kb_layout=us
          kb_options=hungarian_letters:huletters
          kb_variant=altgr-intl
          repeat_delay=250
          repeat_rate=50
        }

        misc {
          background_color=rgb(231e18)
          focus_on_activate=true
        }
      '' + lib.strings.optionalString config.programs.swaylock.enable ''
        bind=,XF86Launch5,exec,${swaylock} -S
        bind=,XF86Launch4,exec,${swaylock} -S
        bind=SUPER,backspace,exec,${swaylock} -S

      '' + lib.strings.optionalString config.services.playerctld.enable ''
        # Media control
        bind=,XF86AudioNext,exec,${playerctl} next
        bind=,XF86AudioPrev,exec,${playerctl} previous
        bind=,XF86AudioPlay,exec,${playerctl} play-pause
        bind=,XF86AudioStop,exec,${playerctl} stop
        bind=ALT,XF86AudioNext,exec,${playerctld} shift
        bind=ALT,XF86AudioPrev,exec,${playerctld} unshift
        bind=ALT,XF86AudioPlay,exec,systemctl --user restart playerctld
      '' + ''
        # Program bindings
        bind=SUPER,Return,exec,${terminal}
        bind=SUPER,e,exec,${editor}
        bind=SUPERSHIFT,e,exec,${vanilla_emacs}
        bind=SUPER,w,exec,${browser}
        # bind=SUPER,l,exec,hyprlock
        bind=SUPERSHIFT,w,exec,${brave}
        # bind=SUPER,r,exec,${terminal-exec} "zsh -c -i 'y'"
        bind=SUPER,r,exec,${terminal-exec} yazi
        bind=SUPERSHIFT,C,exec,${terminal-exec} sudo nmtui
        bind=SUPERSHIFT, R, exec,${hyprctl} reload
        # Brightness control (only works if the system has lightd)
        bind=,XF86MonBrightnessUp,exec,brightnessctl set 5%+
        bind=,XF86MonBrightnessDown,exec,brightnessctl set 5%-
        # Volume
        bind=,XF86AudioRaiseVolume,exec,${pactl} set-sink-volume @DEFAULT_SINK@ +5%
        bind=,XF86AudioLowerVolume,exec,${pactl} set-sink-volume @DEFAULT_SINK@ -5%
        bind=,XF86AudioMute,exec,${pactl} set-sink-mute @DEFAULT_SINK@ toggle
        bind=SHIFT,XF86AudioMute,exec,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle
        bind=,XF86AudioMicMute,exec,${pactl} set-source-mute @DEFAULT_SOURCE@ toggle
        # Screenshotting
        bind=,Print,exec,${grimblast} --notify copy output
        bind=SHIFT,Print,exec,${grimblast} --notify copy active
        bind=CONTROL,Print,exec,${grimblast} --notify copy screen
        bind=SUPER,Print,exec,${grimblast} --notify copy window
        bind=ALT,Print,exec,${grimblast} --freeze --notify copy area
        bind=SUPERSHIFT,p,exec,${grimblast} --freeze --notify copy area
      '' + lib.strings.optionalString config.programs.wofi.enable ''
        bind=SUPER,x,exec,${wofi} -S drun -x 10 -y 10 -W 25% -H 60%
        bind=SUPER,d,exec,${wofi} -S drun
        bind=SUPERSHIFT,N,exec, wofi-shutdown
        bind=SUPERSHIFT,v,exec, wofi-vpn
        bind=SUPERSHIFT,d,exec,${wofi} -S drun
        bind=SUPER, V, exec, cliphist list | wofi --dmenu | cliphist decode | wl-copy
      '' + ''
        # Passthrough mode (e.g. for VNC)
        bind=SUPER,P,submap,passthrough
        submap=passthrough
        bind=SUPER,P,submap,reset
        submap=reset

        $scratchpadsize = size 80% 85%

        # bind=SUPER,Z,exec,if hyprctl clients | grep scratch_term; then echo "scratch_term respawn not needed"; else alacritty --class scratch_term; fi
        bind=SUPER,Z,exec,if hyprctl clients | grep scratch_term; then echo "scratch_term respawn not needed"; else ghostty --class scratch_term; fi
        bind=SUPER,Z,togglespecialworkspace,scratch_term

        $scratch_term = class:^(scratch_term)$
        windowrulev2 = float,$scratch_term
        windowrulev2 = $scratchpadsize,$scratch_term
        windowrulev2 = workspace special:scratch_term ,$scratch_term
        windowrulev2 = center,$scratch_term

        bind=SUPER,B,exec,if hyprctl clients | grep scratch_emacs; then echo "scratch_emacs respawn not needed"; else emacsclient -c --frame-parameters='(quote (name . "scratch_emacs"))'; fi

        bind=SUPER,B,togglespecialworkspace,scratch_emacs

        # $scratch_emacs = class:^(scratch_emacs)$
        $scratch_emacs = workspace:special:scratch_emacs
        windowrulev2 = float,$scratch_emacs
        windowrulev2 = $scratchpadsize,$scratch_emacs
        windowrulev2 = workspace special:scratch_emacs ,$scratch_emacs
        windowrulev2 = center,$scratch_emacs

      '';
    };

    # Stolen from https://github.com/alebastr/sway-systemd/commit/0fdb2c4b10beb6079acd6073c5b3014bd58d3b74
    systemd.user.targets.hyprland-session-shutdown = {
      Unit = {
        Description = "Shutdown running Hyprland session";
        DefaultDependencies = "no";
        StopWhenUnneeded = "true";

        Conflicts = [
          "graphical-session.target"
          "graphical-session-pre.target"
          "hyprland-session.target"
        ];
        After = [
          "graphical-session.target"
          "graphical-session-pre.target"
          "hyprland-session.target"
        ];
      };
    };
  };
}
