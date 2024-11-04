{ config, lib, pkgs, inputs, ... }:

{
  imports = [
    ../common
    ../common/wayland-wm
    ./tty-init.nix
    ./basic-binds.nix
    ./systemd-fixes.nix
  ];

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
  # TODO set up cliphist
  home.packages = with pkgs; [ cliphist hyprpicker brightnessctl xorg.xhost ];

  wayland.windowManager.hyprland = {
    enable = true;
    package =
      inputs.hyprland.packages.${pkgs.stdenv.hostPlatform.system}.hyprland;
    settings = {
      monitor = map (m:
        let
          resolution = "${toString m.width}x${toString m.height}@${
              toString m.refreshRate
            }";
          position = "${toString m.x}x${toString m.y}";
        in "${m.name},${
          if m.enabled then "${resolution},${position},1" else "disable"
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
      terminal = "${pkgs.kitty}/bin/kitty";
      browser = "${pkgs.librewolf}/bin/librewolf";
      brave = "${pkgs.brave}/bin/brave";
      editor = "${pkgs.emacs}/bin/emacsclient -c";
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
        col.shadow=rgba(231e1899)
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
      bind=SUPER,w,exec,${browser}
      bind=SUPERSHIFT,w,exec,${brave}
      bind=SUPER,r,exec,${terminal} lf
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

      bind=SUPER,Z,exec,if hyprctl clients | grep scratch_term; then echo "scratch_term respawn not needed"; else alacritty --class scratch_term; fi
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
}
