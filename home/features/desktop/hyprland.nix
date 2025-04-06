{ config, lib, inputs, pkgs, ... }:
with lib;
let
  cfg = config.features.desktop.hyprland;
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
  options.features.desktop.hyprland.enable = mkEnableOption "hyprland config";

  config = mkIf cfg.enable {

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

    wayland.windowManager.hyprland.settings = {

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
        (map (n: "SUPERSHIFT,${n},movetoworkspacesilent,name:${n}") workspaces)
        ++
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
          "SUPERCONTROLSHIFT,${key},movewindow,mon:${direction}") directions) ++
        # Move workspace to other monitor
        (lib.mapAttrsToList (key: direction:
          "SUPERALT,${key},movecurrentworkspacetomonitor,${direction}")
          directions);
    };
    wayland.windowManager.hyprland = {
      package =
        inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
      enable = true;

      settings = {
        monitor = map (m:
          let
            resolution = "${toString m.width}x${toString m.height}@${
                toString m.refreshRate
              }";
            position = if m.x == null && m.y == null then
              "auto"
            else
              "${toString (if m.x != null then m.x else 0)}x${
                toString (if m.y != null then m.y else 0)
              }";
            transform =
              if m.transform != null then ",transform, ${m.transform}" else "";
          in "${m.name},${
            if m.enabled then
              "${resolution},${position},1${transform}"
            else
              "disable"
          }") (config.monitors);
        workspace = map (m: "${m.name},${m.workspace}")
          (lib.filter (m: m.enabled && m.workspace != null) config.monitors);
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
        terminal = "${pkgs.ghostty}/bin/ghostty";
        terminal-exec = "${pkgs.ghostty}/bin/ghostty -e";
        browser = "${pkgs.librewolf}/bin/librewolf";
        filemanager = "${pkgs.yazi}/bin/yazi";

        brave = "${pkgs.brave}/bin/brave";
        editor = "${pkgs.emacs}/bin/emacsclient -c";
        vanilla_emacs = "${pkgs.emacs}/bin/emacsclient -s vanilla -c";
        hyprctl = "${pkgs.hyprland}/bin/hyprctl";
      in ''
        exec=${pkgs.swaybg}/bin/swaybg -i ${config.wallpaper} --mode fill
      '' + ''
        exec-once=wl-paste --type text --watch cliphist store # Stores only text data
        exec-once=wl-paste --type image --watch cliphist store # Stores only image data
        exec-once=${pkgs.libsForQt5.polkit-kde-agent}/libexec/polkit-kde-authentication-agent-1
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
        bind=SUPERSHIFT,w,exec,${brave}
        # bind=SUPER,r,exec,${terminal-exec} "zsh -c -i 'y'"
        bind=SUPER,r,exec,${terminal-exec} yazi
        bind=SUPERSHIFT,C,exec,${terminal} sudo nmtui
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

    # wayland.windowManager.hyprland.settings = {
    #   xwayland = {
    #     force_zero_scaling = true;
    #   };

    #   exec-once = [
    #     "waybar"
    #     "hyprpaper"
    #     "hypridle"
    #     "wl-paste -p -t text --watch clipman store -P --histpath=\"~/.local/share/clipman-primary.json\""
    #   ];

    #   env = [
    #     "XCURSOR_SIZE,32"
    #     "WLR_NO_HARDWARE_CURSORS,1"
    #     "GTK_THEME,Dracula"
    #   ];

    #   input = {
    #     kb_layout = "de,us";
    #     kb_variant = "";
    #     kb_model = "";
    #     kb_rules = "";
    #     kb_options = "ctrl:nocaps";
    #     follow_mouse = 1;

    #     touchpad = {
    #       natural_scroll = true;
    #     };

    #     sensitivity = 0;
    #   };

    #   general = {
    #     gaps_in = 5;
    #     gaps_out = 5;
    #     border_size = 1;
    #     "col.active_border" = "rgba(9742b5ee) rgba(9742b5ee) 45deg";
    #     "col.inactive_border" = "rgba(595959aa)";
    #     layout = "dwindle";
    #   };

    #   decoration = {
    #     "col.shadow" = "rgba(1E202966)";
    #     drop_shadow = true;
    #     shadow_range = 60;
    #     shadow_offset = "1 2";
    #     shadow_render_power = 3;
    #     shadow_scale = 0.97;
    #     rounding = 8;
    #     blur = {
    #       enabled = true;
    #       size = 3;
    #       passes = 3;
    #     };
    #     active_opacity = 0.9;
    #     inactive_opacity = 0.5;
    #   };

    #   animations = {
    #     enabled = true;
    #     bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
    #     animation = [
    #       "windows, 1, 7, myBezier"
    #       "windowsOut, 1, 7, default, popin 80%"
    #       "border, 1, 10, default"
    #       "borderangle, 1, 8, default"
    #       "fade, 1, 7, default"
    #       "workspaces, 1, 6, default"
    #     ];
    #   };

    #   dwindle = {
    #     pseudotile = true;
    #     preserve_split = true;
    #   };

    #   master = {};

    #   gestures = {
    #     workspace_swipe = false;
    #   };

    #   windowrule = [
    #     "float, file_progress"
    #     "float, confirm"
    #     "float, dialog"
    #     "float, download"
    #     "float, notification"
    #     "float, error"
    #     "float, splash"
    #     "float, confirmreset"
    #     "float, title:Open File"
    #     "float, title:branchdialog"
    #     "float, Lxappearance"
    #     "float, Wofi"
    #     "float, dunst"
    #     "animation none,Wofi"
    #     "float,viewnior"
    #     "float,feh"
    #     "float, pavucontrol-qt"
    #     "float, pavucontrol"
    #     "float, file-roller"
    #     "fullscreen, wlogout"
    #     "float, title:wlogout"
    #     "fullscreen, title:wlogout"
    #     "idleinhibit focus, mpv"
    #     "idleinhibit fullscreen, firefox"
    #     "float, title:^(Media viewer)$"
    #     "float, title:^(Volume Control)$"
    #     "float, title:^(Picture-in-Picture)$"
    #     "size 800 600, title:^(Volume Control)$"
    #     "move 75 44%, title:^(Volume Control)$"
    #   ];

    #   "$mainMod" = "SUPER";

    #   bind = [
    #     "$mainMod, return, exec, kitty -e zellij-ps"
    #     "$mainMod, t, exec, kitty -e fish -c 'neofetch; exec fish'"
    #     "$mainMod SHIFT, e, exec, kitty -e zellij_nvim"
    #     "$mainMod, o, exec, thunar"
    #     "$mainMod, Escape, exec, wlogout -p layer-shell"
    #     "$mainMod, Space, togglefloating"
    #     "$mainMod, q, killactive"
    #     "$mainMod, M, exit"
    #     "$mainMod, F, fullscreen"
    #     "$mainMod, V, togglefloating"
    #     "$mainMod, D, exec, wofi --show drun --allow-images"
    #     "$mainMod SHIFT, S, exec, bemoji"
    #     "$mainMod, P, exec, wofi-pass"
    #     "$mainMod SHIFT, P, pseudo"
    #     "$mainMod, J, togglesplit"
    #     "$mainMod, left, movefocus, l"
    #     "$mainMod, right, movefocus, r"
    #     "$mainMod, up, movefocus, u"
    #     "$mainMod, down, movefocus, d"
    #     "$mainMod, 1, workspace, 1"
    #     "$mainMod, 2, workspace, 2"
    #     "$mainMod, 3, workspace, 3"
    #     "$mainMod, 4, workspace, 4"
    #     "$mainMod, 5, workspace, 5"
    #     "$mainMod, 6, workspace, 6"
    #     "$mainMod, 7, workspace, 7"
    #     "$mainMod, 8, workspace, 8"
    #     "$mainMod, 9, workspace, 9"
    #     "$mainMod, 0, workspace, 10"
    #     "$mainMod SHIFT, 1, movetoworkspace, 1"
    #     "$mainMod SHIFT, 2, movetoworkspace, 2"
    #     "$mainMod SHIFT, 3, movetoworkspace, 3"
    #     "$mainMod SHIFT, 4, movetoworkspace, 4"
    #     "$mainMod SHIFT, 5, movetoworkspace, 5"
    #     "$mainMod SHIFT, 6, movetoworkspace, 6"
    #     "$mainMod SHIFT, 7, movetoworkspace, 7"
    #     "$mainMod SHIFT, 8, movetoworkspace, 8"
    #     "$mainMod SHIFT, 9, movetoworkspace, 9"
    #     "$mainMod SHIFT, 0, movetoworkspace, 10"
    #     "$mainMod, mouse_down, workspace, e+1"
    #     "$mainMod, mouse_up, workspace, e-1"
    #   ];

    #   bindm = [
    #     "$mainMod, mouse:272, movewindow"
    #     "$mainMod, mouse:273, resizewindow"
    #   ];

    #   windowrulev2 = [
    #     "workspace 1,class:(Emacs)"
    #     "workspace 3,opacity 1.0, class:(brave-browser)"
    #     "workspace 4,class:(com.obsproject.Studio)"
    #   ];
    # };

    # Stolen from https://github.com/alebastr/sway-systemd/commit/0fdb2c4b10beb6079acd6073c5b3014bd58d3b74
    # systemd.user.targets.hyprland-session-shutdown = {
    #   Unit = {
    #     Description = "Shutdown running Hyprland session";
    #     DefaultDependencies = "no";
    #     StopWhenUnneeded = "true";

    #     Conflicts = [
    #       "graphical-session.target"
    #       "graphical-session-pre.target"
    #       "hyprland-session.target"
    #     ];
    #     After = [
    #       "graphical-session.target"
    #       "graphical-session-pre.target"
    #       "hyprland-session.target"
    #     ];
    #   };
    # };
  };
}
